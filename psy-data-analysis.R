################################################################################
# Class:      Psychology Collaboration
# Topic:      Tenure and Job Performance
#
# Sources:    SPSS File: "Personality-Performance-Turnover-Chaehan So.sav"
#
################################################################################
# mode <- "new"
mode <- "old"

# Important:
# BEFORE script, sync the google drive folder, otherwise data will not be found!

# load libraries
# detach("package:machinelearningtools", character.only = TRUE)
# devtools::install_github("agilebean/machinelearningtools", force = TRUE)

libraries <- c(
  "magrittr"
  , "sjlabelled" # read SPSS
  , "caret", "doParallel"
  , "RColorBrewer"
  , "machinelearningtools"
  , "knitr"
  , "RPushbullet", "beepr"
  , "tidyverse"
)
sapply(libraries, require, character.only = TRUE)

source("_labels.R")

# nominal <- FALSE # with ordinal as ORDERED factors
nominal <- TRUE # with ordinal as NOMINAL factor

seed <- 171

# cross-validation repetitions
# CV.REPEATS <- 2
# CV.REPEATS <- 10
CV.REPEATS <- 100

# try first x rows of training set
# TRY.FIRST <- NULL
TRY.FIRST <- 50
# TRY.FIRST <- 100

# split ratio
SPLIT.RATIO <- 1.0

# imputation method
IMPUTE.METHOD <- "noimpute"
# IMPUTE.METHOD <- "knnImpute"
# IMPUTE.METHOD <- "bagImpute"

# prefix
PREFIX <- "data/models.list"
# PREFIX <- "data/testruns/models.list"

#######################################################################
# define features
#######################################################################
get_features <- function(data, target_label, features_set_label) {

  data %>%
    select(-(target_label),
           -starts_with("TO"),
           -starts_with("PERF"),
           -starts_with("LIFE"),
           -job
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
  # , "knn"
  # , "kknn"
  # , "gbm"
  # , "rf" # 754s/100rep
  # , "ranger"
  # , "xgbTree" # 377s/100rep
  # , "xgbLinear" # 496s/100rep
  # , "svmLinear"
  , "svmRadial"
)


# clusterOff(cluster.new)

#######################################################################
# MAIN
#######################################################################


#######################################################################
if (mode == "new") {

  # repeated cv
  training.configuration <- trainControl(
    method = "repeatedcv", number = 10
    , repeats = CV.REPEATS
    , savePredictions = "final"
    )

  ###################################################
  # 1. Data Acquistion - includes 2.2 Data Cleaning
  ###################################################

  if (IMPUTE.METHOD != "noimpute") {
    dataset.label <- "data/dataset.rds" %>% print
  } else {
    dataset.label <- "data/dataset.NA.rds" %>% print
  }

  data.new <- readRDS(dataset.label) %T>% print

  if (IMPUTE.METHOD != "noimpute") {

    system.time(
      # tricky tricky: predict throws ERROR (variable is of class NULL)
      # if factors contain NA > remove_unused_variables
      dataset.imputed <- data.new %>%
        preProcess(method = IMPUTE.METHOD) %>%
        predict(newdata = data.new)
    ) %T>% print
    data.new <- dataset.imputed %>% na.omit

  }

  # target_label <- "PERF10"
  # features_set_label <- "big5items"
  # features.labels <- data.new %>% get_features(target_label, features_set_label)
  # # job_label <-  "sales"
  # # job_label <-  "R&D"
  # job_label <- "support"
  # # job_label <-  "all"

  # desc stats
  data.new %>%
    group_by(job) %>%
    tally() %>%
    filter(job %in% c(1, 10))

  time.total <- system.time(
    ############ START
    model.permutations.list <- model.permutations.labels %>%

      pmap(function(target_label, features_set_label, job_label) {

        # models.list.name <- output_filename(
        #   PREFIX, target_label, features_set_label, CV.REPEATS, IMPUTE.METHOD)

        models.list.name <- output_filename(
          PREFIX,
          c(target_label, features_set_label, job_label),
          cv_repeats = CV.REPEATS, impute_method = IMPUTE.METHOD
        ) %>% print

        features.labels <- data.new %>% get_features(target_label, features_set_label)

        # select variables - for different targets, #NA can differ
        dataset <- data.new %>%
          select(job, target_label, features.labels) %>%
          {
            if (job_label == "sales") {
              filter(., job == 1)
            } else if (job_label == "R&D") {
              filter(., job == 10)
            } else if (job_label == "support") {
              filter(., job != 1 & job != 10)
            } else {
              .
            }
          } %>%
          select(-job) %>%
          na.omit %T>%
          print

        ########################################
        ## 2.4 Split the data
        ########################################
        set.seed(seed)
        training.index <- createDataPartition(
          dataset[[target_label]], p = SPLIT.RATIO, list = FALSE
        )
        training.set <- dataset[training.index, ] %T>% print
        # if split_ratio == 100%, then create no testing.set
        testing.set <- if (SPLIT.RATIO != 1.0) dataset[-training.index, ] else NULL
        testing.set

        # benchmark ml algorithms
        models.list <- benchmark_algorithms(

          target_label = target_label,
          features_labels = features.labels,
          training_set = training.set,
          testing_set = testing.set,
          impute_method = IMPUTE.METHOD,
          algorithm_list = algorithm.list,
          glm_family = "gaussian",
          training_configuration = training.configuration,
          seed = seed,
          # split_ratio = SPLIT.RATIO,
          cv_repeats = CV.REPEATS,
          try_first = TRY.FIRST,
          models_list_name = models.list.name

        )
        return(models.list)
      })
    ############ END
  ) %T>% { push_message(.["elapsed"], model.permutations.strings ) }

}

## 4.1 Training Set Performance
########################################
# get model in model.permutations.list by model index
model.index = 1
model.index.labels <- model.permutations.labels %>% .[model.index,] %T>% print
target_label <- model.index.labels$target_label
features_set_label <- model.index.labels$features_set_label
job_label <- model.index.labels$job_label
CV.REPEATS <- 100

# prefix
models.list.name <- output_filename(
  PREFIX,
  c(target_label, features_set_label, job_label),
  paste0(CV.REPEATS, "repeats"), impute_method = IMPUTE.METHOD
) %>% print

# get model in model.permutations.labels by model index
models.list <- readRDS(models.list.name)
# models.list <- readRDS("data/models.list.PERF10.big5items.100repeats.noimpute.rds")


# training set performance
models.metrics <- models.list %>% get_model_metrics %T>% print
# models.metrics <- models.list %>% get_model_metrics(palette = "Dark2") %T>% print

models.metrics$metric1.resamples.boxplots  +
  theme(text = element_text(family = 'Gill Sans'))

models.metrics$metric2.resamples.boxplots

models.list.stripped <- models.list %>%
  purrr::list_modify(target.label = NULL, testing.set = NULL)
models.list.stripped %>% resamples %>% dotplot
models.list.stripped %>% resamples %>% .$values


dataset <- models.list$testing.set
dataset %<>% select(-PERF09)
pc <- dataset %>% prcomp


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

data.new %>% nearZeroVar(saveMetrics = TRUE)

get_featureset <- function(data,
                           target_label = NULL,
                           featureset_labels = NULL,
                           select_starts = NULL) {

  data %>%
    dplyr::select(!!rlang::sym(target_label)) %>%

    {
      if (!is.null(featureset_labels)) {
        cbind(.,
              data %>%
                dplyr::select(!!!rlang::syms(featureset_labels))
        )
      } else { . }
    } %>%
    {
      if (!is.null(select_starts)) {

        cbind(.,
              map_dfc(select_starts, function(start_keyword) {
                data %<>%
                  select(starts_with(start_keyword))
              })
        )

      } else { . }
    } %>%
    select(-job) %>%
    as_tibble()
}

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

