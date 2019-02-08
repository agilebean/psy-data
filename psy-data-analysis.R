################################################################################
# Class:      Psychology Collaboration
# Topic:      Tenure and Job Performance
#
# Sources:    SPSS File: "Personality-Performance-Turnover-Chaehan So.sav"
#
################################################################################
# clear the workspace
rm(list=ls())

# mode <- "new"
mode <- "old"

dataset.label <- paste0(c("data/dataset", "rds"), collapse = ".")

# load libraries
# devtools::install_github("agilebean/machinelearningtools", force = TRUE)
libraries <- c("dplyr", "magrittr", "tidyverse"
               , "sjlabelled" # read SPSS
               , "caret", "doParallel"
               , "stargazer", "DataExplorer", "skimr"
               , "machinelearningtools"
               , "knitr", "pander"
)
sapply(libraries, require, character.only = TRUE)

target.label.list <- c("PERF09", "PERF.all", "TO.all")
features.set.list <- c("big5items", "big5composites")

model.permutations.list <- crossing(target_label = target.label.list, 
                                    features_set = features.set.list)

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

#######################################################################
# TRAIN model permutations
#######################################################################
train_model_permutations <- function(target_label, features_set, 
                                     data_set, algorithm_list, training_configuration, 
                                     seed = 17, split_ratio = 0.80, try_first = NULL
                                     ) {
  # define output filename
  models.list.name <- paste0(c("data/models.list", target_label, features_set, "rds"), 
                             collapse = ".") %T>% print
  
  ########################################
  ## 2.3 Select the target & features
  ########################################
  target_label %>% print
  features_set %>% print
  
  ########################################
  ## 2.4 Split the data
  ########################################
  # shuffle data - short version:
  set.seed(seed)
  dataset <- data_set %>% nrow %>% sample %>% data_set[.,]
  
  # dataset subsetting for tibble: [[
  set.seed(seed)
  training.index <- createDataPartition(dataset[[target_label]], p = split_ratio, list = FALSE)
  testing.set <- dataset[-training.index, ]
  training.set <- dataset[training.index, ]
  
  ########################################
  # 3.2: Select the features & formula
  ########################################
  # define features
  features <- get_features(target_label, features_set, data_set)

  # define formula
  formula1 <- set_formula(target_label, features)

  ########################################
  # 3.3: Train the models
  ########################################
  models.list <- list()
  
  system.time(
    models.list <- algorithm_list %>%
      map(function(algorithm_label) {
        train(formula1
              , method = algorithm_label
              , data = if (is.null(try_first)) training.set else head(training.set, try_first)
              , preProcess = c("center", "scale")
              , trControl = training_configuration
        )
      }) %>%
      setNames(algorithm_list) 
  ) %>% print

  ########################################
  # 3.4: Postprocess the models
  ########################################
  # add target.label & testing.set to models.list
  models.list$target.label <- target_label
  models.list$testing.set <- testing.set
  # 
  # # save the models.list
  models.list %>% saveRDS(models.list.name)
  
  return(models.list)
}

#######################################################################
# 1. Data Acquistion - includes 2.2 Data Cleaning
#######################################################################
dataset <- readRDS(dataset.label) %T>% print

########################################
# 3.1: Select the models
########################################
algorithm.list <- c(
  "lm"
  ,"glm"
  , "knn"
  , "gbm"
  , "rf"
  , "ranger"
  , "xgbTree"
  , "xgbLinear"
  , "svmLinear"
  , "svmRadial"
)


#######################################################################
# MAIN
#######################################################################
if (mode == "new") {
  
  cluster.new <- clusterOn()
  
  system.time(
    result <- model.permutations.list %>% 
      pmap(train_model_permutations, 
           data_set = dataset,
           algorithm_list = algorithm.list,
           training_configuration = trainControl(method = "repeatedcv", number = 10, repeats = 10), 
           try_first = NULL
      )
  ) %>% print

  # stop cluster if exists
  if (nrow(showConnections()) != 0) stopCluster(cluster.new) 
  
  } else if (mode == "old") {
    
    models.list <- get_models_list(model.permutations.list, 1)
    models.list %>% head(-2) %>% resamples %>% dotplot
  }


################################################################################
# 4. Evaluate Models
################################################################################

########################################
## 4.1 Training Set Performance
########################################
# get model
models.list <- get_models_list(model.permutations.list, 5)

# training set performance
models.metrics <- models.list %>% get_model_metrics %T>% print

# set color palettes (default = "Set1")
models.list %>% get_model_metrics(palette = "Dark2")

# get model comparison
models.list %>% head(-2) %>% resamples %>% dotplot
models.list %>% head(-2) %>% resamples %>% bwplot

########################################
## 4.2 Testing Set Performance
########################################
# RMSE for all models on testing set
models.metrics$RMSE.testing
# training vs. testing set performance: RMSE
models.metrics$RMSE.all 

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

