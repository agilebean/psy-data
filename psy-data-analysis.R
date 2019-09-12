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

# Important:
# BEFORE script, sync the google drive folder, otherwise data will not be found!

# load libraries
# devtools::install_github("agilebean/machinelearningtools", force = TRUE)
detach("package:machinelearningtools", unload = TRUE)

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

# Gcloud:
## RPushbullet must be initialized: pbSetup() + get Access Token from website
## o.lgWvoSgOZ0is96arIc3sFZC3Y2kD2J8i

target.label.list <- c("LIFE_S_R", "PERF09", "PERF10",  "PERF11")
# target.label.list <- c("PERF09")
features.set.labels.list <- c("big5items", "big5composites")
# features.set.labels.list <- c("big5composites")
# features.set.labels.list <- c("big5items")

model.permutations.labels <- crossing(
  target_label = target.label.list,
  features_set_label = features.set.labels.list
  )

model.permutation.string <- model.permutations.labels %>%
  pmap_chr(function(target_label, features_set_label) {
    paste(target_label, features_set_label, sep = "-")
  })

# nominal <- FALSE # with ordinal as ORDERED factors
nominal <- TRUE # with ordinal as NOMINAL factor

seed <- 17

# cross-validation repetitions
CV.REPEATS <- 2
# CV.REPEATS <- 10
# CV.REPEATS <- 100

# try first x rows of training set
# TRY.FIRST <- NULL
TRY.FIRST <- 50

IMPUTE.METHOD <- NULL
# IMPUTE.METHOD <- "knnImpute"
# IMPUTE.METHOD <- "bagImpute"

# prefix
PREFIX <- "data/models.list"
# PREFIX <- "data/testruns/models.list"

#######################################################################
# define features
#######################################################################
get_features <- function(target_label, features_set_label, data) {

  data %>%
    select(-target_label,
           -starts_with("TO"),
           -starts_with("PERF")
    ) %>%
    {
      if (features_set_label == "big5items") {
        # remove composite scores - equivalent to (-nn, -ee, -oo, -aa, -cc)
        select(., -matches("(oo|cc|ee|aa|nn)$"))

      } else if (features_set_label == "big5composites") {
        # remove Big5 items
        select(., -matches(".*(1|2|3|4|5|6)"))

      } else { . }
    } %>%
    names
}


########################################
# 3.1: Select the models
########################################
algorithm.list <- c(
  "lm"
  # ,"glmnet"
  , "knn"
  # , "kknn"
  # , "gbm"
  # , "rf" # 754s/100rep
  # , "ranger"
  # , "xgbTree" # 377s/100rep
  # , "xgbLinear" # 496s/100rep
  # , "svmLinear"
  # , "svmRadial"
)

# clusterOff(cluster.new)

#######################################################################
# MAIN
#######################################################################
if (mode == "new") {

  # repeated cv
  training.configuration <- trainControl(
    method = "repeatedcv", number = 10
    , repeats = CV.REPEATS
    , savePredictions = "final",
    # , returnResamp = "all"
    )

  ###################################################
  # 1. Data Acquistion - includes 2.2 Data Cleaning
  ###################################################

  if (!is.null(IMPUTE.METHOD)) {
    dataset.label <- "data/dataset.rds" %>% print
  } else {
    dataset.label <- "data/dataset.NA.rds" %>% print
  }

  data.new <- readRDS(dataset.label) %T>% print

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

    dataset <- data.new
  }

  time.total <- system.time(
    ############ START
    model.permutations.list <- model.permutations.labels %>%

      pmap(function(target_label, features_set_label) {

        models.list.name <- output_filename(
          PREFIX, target_label, features_set_label, CV.REPEATS, IMPUTE.METHOD)
        # models.list.name <- "data/models.list.PERF09.big5composites.100repeats.knnImpute.rds"
        # target_label <- "LIFE_S_R"
        # features_set_label <- "big5composites"
        #
        features.labels <- get_features(target_label, features_set_label, dataset)

        # define formula
        formula1 <- set_formula(target_label, features.labels)
        # formula1 <- NULL

        models.list <- benchmark_algorithms(
          target_label = target_label,
          features = features.labels,
          formula_input = NULL,
          data = dataset,
          impute_method = IMPUTE.METHOD,
          algorithm_list = algorithm.list,
          glm_family = "gaussian",
          training_configuration = training.configuration,
          cv_repeats = CV.REPEATS,
          try_first = TRY.FIRST,
          models_list_name = models.list.name
        )
        return(models.list)
      })
    ############ END
  ) %T>% { push_message(.["elapsed"], model.permutation.string ) }

}

models.list
models.list <- readRDS("data/models.list.LIFE_S_R.big5items.2repeats.noimpute.rds")
models.list %>% machinelearningtools::get_model_metrics()

################################################################################
# 4. Evaluate Models
################################################################################

get_testingset_performance <- function(
  models_list, target_label = NULL, testing_set = NULL) {

  # remove target.label + testing.set from models.list
  if (!is.null(models_list$target.label) & !is.null(models_list$testing.set)) {

    target.label <- models_list$target.label
    testing.set <- models_list$testing.set
    models_list %<>% purrr::list_modify("target.label" = NULL, "testing.set" = NULL)

  } else if (!is.null(target_label) & !is.null(testing_set)) {

    target.label <- target_label
    testing.set <- testing_set
  }

  features.labels <- testing.set %>% select(-target.label) %>% names

  observed <- testing.set[[target.label]]

  if (is.factor(observed)) {

    models_list %>%
      map(
        function(model_object) {
          # print(model_object$method)
          if (contains_factors(testing.set) & !handles_factors(model_object$method)) {
            formula1 <- set_formula(target.label, features.labels)
            testing.set <- model.matrix(formula1, data = testing.set)
          }
          model_object %>%
            # estimate target in the testing set
            predict(., newdata = testing.set) %>%
            confusionMatrix(., observed) %>%
            .$overall %>%
            # tricky: convert first to dataframe > can select column names
            map_df(1) %>% select(Accuracy, Kappa)
        }
      ) %>%
      bind_rows(.id = "model") %>%
      setNames(c("model", "Acc.testing", "Kappa.testing"))

  } else if (is.numeric(observed)) {

    models_list %>%
      # caret::predict() can take a list of train objects as input
      predict(testing.set) %>%
      map_df(function(predicted) {
        c(sqrt(mean( (observed - predicted)^2)),
          # R2 = regression SS / TSS > https://stackoverflow.com/a/40901487/7769076
          sum((predicted - mean(predicted))^2) / sum((observed - mean(observed))^2))
      }) %>%
      t %>%
      as_tibble(rownames = "model") %>%
      rename(RMSE.testing = V1, Rsquared.testing = V2) %>%
      arrange(RMSE.testing)
  }
}

########################################
## 4.1 Training Set Performance
########################################
# get model in model.permutations.list by model index
if (mode == "new") {
  model.index = 1
  models.list <- model.permutations.list[[model.index]]
  models.list <- readRDS("data/models.list.LIFE_S_R.big5composites.2repeats.noimpute.rds")
  models.list %>% head(-2) %>% resamples %>% bwplot

} else if (mode == "old") {

  # get model for first permutation
  model.index = 1
  model.index.labels <- model.permutations.labels %>% .[model.index,] %T>% print
  target_label <- model.index.labels$target_label
  features_set_label <- model.index.labels$features_set_label

  # prefix
  models.list.name <- output_filename(
    PREFIX, target_label, features_set_label, CV.REPEATS, IMPUTE.METHOD)

  # get model in model.permutations.labels by model index
  models.list <- readRDS(models.list.name)

  models.list %>%
    purrr::list_modify(target.label = NULL, testing.set = NULL) %>%
    resamples %>% dotplot
}

# training set performance
models.metrics <- models.list %>% get_model_metrics %T>% print
# models.metrics <- models.list %>% get_model_metrics(palette = "Dark2") %T>% print

models.metrics$metric1.resamples.boxplots  +
  theme(text = element_text(family = 'Gill Sans'))


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

