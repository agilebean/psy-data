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
detach("package:machinelearningtools", character.only = TRUE)

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

# target.label.list <- c("LIFE_S_R", "PERF09", "PERF10",  "PERF11")
target.label.list <- c("PERF09", "PERF10",  "PERF11")
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

seed <- 171

# cross-validation repetitions
# CV.REPEATS <- 2
# CV.REPEATS <- 10
CV.REPEATS <- 100

# try first x rows of training set
TRY.FIRST <- NULL
# TRY.FIRST <- 50
# TRY.FIRST <- 200

# split ratio
SPLIT.RATIO <- 0.8

# imputation method
IMPUTE.METHOD <- NULL
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
    select(-target_label,
           -starts_with("TO"),
           -starts_with("PERF"),
           -starts_with("LIFE")
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
  ,"glmnet"
  , "knn"
  , "kknn"
  , "gbm"
  , "rf" # 754s/100rep
  , "ranger"
  , "xgbTree" # 377s/100rep
  , "xgbLinear" # 496s/100rep
  , "svmLinear"
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
    data.new <- dataset.imputed %>% na.omit

  }

  time.total <- system.time(
    ############ START
    model.permutations.list <- model.permutations.labels %>%

      pmap(function(target_label, features_set_label) {

        models.list.name <- output_filename(
          PREFIX, target_label, features_set_label, CV.REPEATS, IMPUTE.METHOD)

        features.labels <- data.new %>% get_features(target_label, features_set_label)

        # select variables - for different targets, #NA can differ
        dataset <- data.new %>%
          select(target_label, features.labels) %>%
          na.omit

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
        testing.set %T>% print

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
          split_ratio = SPLIT.RATIO,
          cv_repeats = CV.REPEATS,
          try_first = TRY.FIRST,
          models_list_name = models.list.name

        )
        return(models.list)
      })
    ############ END
  ) %T>% { push_message(.["elapsed"], model.permutation.string ) }

}


## 4.1 Training Set Performance
########################################
# get model in model.permutations.list by model index
model.index = 1
model.index.labels <- model.permutations.labels %>% .[model.index,] %T>% print
target_label <- model.index.labels$target_label
features_set_label <- model.index.labels$features_set_label

# prefix
models.list.name <- output_filename(
  PREFIX, target_label, features_set_label, CV.REPEATS, IMPUTE.METHOD)

# get model in model.permutations.labels by model index
models.list <- readRDS(models.list.name)

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

