################################################################################
# Class:      Psychology Collaboration
# Topic:      Tenure and Job Performance
#
# Sources:    SPSS File: "Personality-Performance-Turnover-Chaehan So.sav"
#
################################################################################
# clear the workspace
rm(list=ls())

mode <- "new"
# mode <- "old"

# load libraries
# devtools::install_github("agilebean/machinelearningtools", force = TRUE)

libraries <- c("magrittr"
               , "sjlabelled" # read SPSS
               , "caret", "doParallel"
               , "RColorBrewer"
               , "machinelearningtools"
               , "knitr"
               , "RPushbullet", "beepr"
               , "tidyverse"
)
sapply(libraries, require, character.only = TRUE)

# target.label.list <- c("PERF09", "PERF.all", "TO.all", "LIFE_S_R")
target.label.list <- c("PERF07", "PERF08", "PERF09")
features.labels.list <- c("big5items", "big5composites")

model.permutations.labels <- crossing(target_label = target.label.list,
                                      features_labels = features.labels.list)

# nominal <- FALSE # with ordinal as ORDERED factors
nominal <- TRUE # with ordinal as NOMINAL factor

seed <- 17


#######################################################################
# define features
#######################################################################
get_features <- function(target_label, features_set, data_set) {

  data_set %>%
    select(-target_label,
           -starts_with("TO"),
           -starts_with("PERF")
    ) %>%
    {
      if (features_set == "big5items") {
        # remove composite scores - equivalent to (-nn, -ee, -oo, -aa, -cc)
        select(., -matches("(oo|cc|ee|aa|nn)$"))

      } else if (features_set == "big5composites") {
        # remove Big5 items
        select(., -matches(".*(1|2|3|4|5|6)"))

      } else { . }
    } %>%
    names %T>% print
}

# define output filename
output_filename <- function(prefix, target_label, features_set,
                            cv_repeats, impute_method) {
  paste0(c(prefix,
           target_label, features_set,
           paste0(cv_repeats, "repeats"),
           { if (!is.null(impute_method)) paste(impute_method)},
           "rds"),
         collapse = ".") %T>% print
}

#######################################################################
# TRAIN model permutations
#######################################################################
train_model <- function(target_label, features_labels,
                        preprocess_configuration = c("center", "scale"),
                        impute_method = NULL,
                        data_set, algorithm_list, training_configuration,
                        seed = 17, split_ratio = 0.80,
                        cv_repeats, try_first = NULL,
                        models_list_name = NULL
) {

  ########################################
  ## 2.3 Select the target & features
  ########################################
  target_label %>% print
  features_labels %>% print

  ########################################
  ## 2.4 Split the data
  ########################################
  # shuffle data - short version:
  set.seed(seed)
  dataset <- data_set %>% nrow %>% sample %>% data_set[.,]

  # select variables
  dataset %<>% select(target_label, features_labels) %>%
    # for non-imputed data, #NA can differ for different targets
    na.omit

  # dataset subsetting for tibble: [[
  set.seed(seed)
  training.index <- createDataPartition(dataset[[target_label]], p = split_ratio, list = FALSE)
  training.set <- dataset[training.index, ]
  testing.set <- dataset[-training.index, ]

  ########################################
  # 3.2: Select the features & formula
  ########################################

  # define formula
  formula1 <- set_formula(target_label, features_labels)

  ########################################
  # 3.3: Train the models
  ########################################
  models.list <- list()

  system.time(
    models.list <- algorithm_list %>%
      map(function(algorithm_label) {

        if (algorithm_label == "rf") {
          train(formula1
                , method = algorithm_label
                , data = if (is.null(try_first)) training.set else head(training.set, try_first)
                , preProcess = preprocess_configuration
                , trControl = training_configuration
                , importance = TRUE
          )
        } else {
          train(formula1
                , method = algorithm_label
                , data = if (is.null(try_first)) training.set else head(training.set, try_first)
                , preProcess = preprocess_configuration
                , trControl = training_configuration
          )
        }
      }) %>%
      setNames(algorithm_list)
  ) %>% beepr::beep()

  ########################################
  # Postprocess the models
  ########################################
  # add target.label & testing.set to models.list
  models.list$target.label <- target_label
  models.list$testing.set <- testing.set
  #
  # save the models.list
  if (is.null(try_first) & !is.null(models_list_name)) {

    models.list %>% saveRDS(models_list_name)

    print(paste("model training results saved in", models_list_name))
  }

  return(models.list)
}


########################################
# 3.1: Select the models
########################################
algorithm.list <- c(
  "lm"
  ,"glm"
  , "knn"
  , "gbm"
  , "rf" # 754s/100rep
  , "ranger"
  , "xgbTree" # 377s/100rep
  , "xgbLinear" # 496s/100rep
  , "svmLinear"
  , "svmRadial"
)

# script-specific implementation
remove_unused_variables <- function(data) {

  data %>%
    # rerun big5 without covariates
    select(-COMNAME, -educa, -gender, -LIFE_S_R, -inf, -sd) %>%
    # remove turnover (TOxx) variables
    select(-starts_with("TO")) %>%
    # remove performance composite index
    select(-PERF.all)
}

#######################################################################
# MAIN
#######################################################################
if (mode == "new") {

  # cross-validation repetitions
  CV.REPEATS <- 2
  # CV.REPEATS <- 10
  # CV.REPEATS <- 100

  # try first x rows of training set
  TRY.FIRST <- 50
  # TRY.FIRST <- NULL

  # IMPUTE.METHOD <- NULL
  # IMPUTE.METHOD <- "knnImpute"
  IMPUTE.METHOD <- "bagImpute"

  # repeated cv
  training.configuration <- trainControl(
    method = "repeatedcv", number = 10, repeats = CV.REPEATS)

  ###################################################
  # 1. Data Acquistion - includes 2.2 Data Cleaning
  ###################################################
  dataset.label <- "data/data.raw.rds" %T>% print
  data.raw <- readRDS(dataset.label)
  # script-specific removal of unused variables
  data.new <- data.raw %>% remove_unused_variables %T>% print

  data.new %<>% .[1:100,]
  if (!is.null(IMPUTE.METHOD)) {

    system.time(
      # tricky tricky: predict throws ERROR (variable is of class NULL)
      # if factors contain NA > remove_unused_variables
      dataset.imputed <- data.new %>%
        preProcess(method = IMPUTE.METHOD) %>%
        predict(newdata = data.new)
    ) %T>% print
    dataset <- dataset.imputed %>% na.omit

  } else {
    # script-specific removal of unused variables
    dataset <- data.new %>% na.omit %T>% print
  }

  cluster.new <- clusterOn(detectCores())

  time.total <- system.time(
    ############ START
    model.permutations.list <- model.permutations.labels %>%
      pmap(function(target_label, features_labels) {

        prefix.models.list <- "data/testruns/models.list"

        models.list.name <- output_filename(
          prefix.models.list, target_label, features_labels, CV.REPEATS, IMPUTE.METHOD)

        features <- get_features(target_label, features_labels, dataset)

        print(c(features_labels, target_label))

        train_model(
          target_label = target_label,
          features = features,
          data_set = dataset,
          impute_method = IMPUTE.METHOD,
          algorithm_list = algorithm.list,
          training_configuration = training.configuration,
          cv_repeats = CV.REPEATS,
          try_first = TRY.FIRST,
          models_list_name = models.list.name
        )
      })
    ############ END
  ) %T>% { push_message(.["elapsed"]) }

  # stop cluster if exists
  if (nrow(showConnections()) != 0) {
    registerDoSEQ()
    stopCluster(cluster.new)
  }

} else if (mode == "old") {

  # get model in model.permutations.labels by model index
  model.permutations.list <- get_models_list(
    model.permutations.labels,
    model_index = 1,
    prefix = prefix.models.list,
    impute_method = IMPUTE.METHOD,
    cv_repeats = CV.REPEATS
  )

  models.list %>%
    purrr::list_modify(target.label = NULL, testing.set = NULL) %>%
    resamples %>% dotplot
}


################################################################################
# 4. Evaluate Models
################################################################################

model.permutations.list <- get_models_list(model.permutations.labels, model_index = 1,
                                           impute_method = IMPUTE.METHOD,
                                           cv_repeats = CV.REPEATS)

model.metrics <- models.list %>% get_model_metrics
model.metrics$metric1.resamples.boxplots  +
  theme(text = element_text(family = 'Gill Sans'))

########################################
## 4.1 Training Set Performance
########################################
# get model in model.permutations.list by model index
model.index <- 1

if (mode == "new") {

  models.list <- result.permutations[[model.index]]

} else if (mode == "old") {

  models.list <- get_models_list(model.permutations.list,
                                 model_index = model.index,
                                 cv_repeats = CV.REPEATS,
                                 impute_method = "knnImpute")
}

# training set performance
models.metrics <- models.list %>% get_model_metrics %T>% print

# set color palettes (default = "Set1")
models.list %>% get_model_metrics(palette = "Dark2")


########################################
## 4.2 Testing Set Performance
########################################
# RMSE for all models on testing set
models.metrics$metrics.testing
# training vs. testing set performance: RMSE
models.metrics$benchmark.all

################################################################################
################################################################################
################################################################################
#
# SCRIBBLE
#
################################################################################
################################################################################




################################################################################
#
# LESSONS LEARNED
#
################################################################################
#
################################################################################
# 1. tidyr::crossing > creates permutations without factor conversion like base::expand.grid
#
# model.permutations.list <- crossing(target_label = target.label.list,
#                                     features_set = features.set.list)
#
################################################################################
# 2. unified handling: conditional inline ggplot statements
#
#   dataset %>%
#   {
#     if (conditions1) {
#       select(., -matches("(oo|cc|ee|aa|nn)$"))
#
#     } else if (condition2") {
#       select(., -matches(".*(1|2|3|4|5|6)"))
#
#     } else { . }
#   } %>% ...
#
################################################################################
# 3. quick prototyping: parametrized training set size
#
#   main_function(..., try_first = NULL) {
#     train(...,
#           data = if (is.null(try_first)) training.set else head(training.set, try_first),
#           ...)
#   }
#
################################################################################
# 4. unified handling: add testing set to model to calculate testing set performance
#
# during training:
#   models.list$target.label <- target_label
#   models.list$testing.set <- testing.set
#
# after training:
#   get_model_metrics(target_label = NULL,
#                     testing_set = NULL, ...) {
#     target.label <- if (!is.null(target_label)) target_label else models_list$target.label
#     testing.set <- if (!is.null(testing_set)) testing_set else models_list$testing.set
#
#     RMSE.testing <- get_rmse_testing(target.label, models.list, testing.set)
#   }
#
# for resamples() must remove these 2 parameters:
#   models.list %>% head(-2) %>% resamples
#
################################################################################
# 5. quick prototyping: set comma before parameter in list
# algorithm.list <- c(
#   "lm"
#   ,"glm"
#   # , "knn"
# )
# stop cluster if exists
# if (nrow(showConnections()) != 0) stopCluster(cluster.new)
#
################################################################################

