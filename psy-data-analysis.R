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
# mode <- "report.single"
# mode <- "report.all"

dataset.label <- paste0(c("data/dataset", "rds"), collapse = ".")

# load libraries
# devtools::install_github("agilebean/machinelearningtools", force = TRUE)
libraries <- c("dplyr", "magrittr", "tidyverse", "purrr"
               , "sjlabelled" # read SPSS
               , "caret", "doParallel"
               , "stargazer", "DataExplorer", "skimr"
               , "machinelearningtools"
               , "knitr", "pander"
)
sapply(libraries, require, character.only = TRUE)

target.label.list <- c("PERF09", "PERF.all")
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
  formula1 <- features %>% 
    paste(collapse = " + ") %>% 
    paste(target_label, "~", .) %>% 
    as.formula %T>% print

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

if (mode == "new") {
  
  cluster.new <- clusterOn()
  
  system.time(
    result <- model.permutations.list %>% 
      pmap(train_model_permutations, 
           data_set = dataset,
           algorithm_list = algorithm.list,
           training_configuration = trainControl(method = "repeatedcv", number = 10, repeats = 3), 
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
models.list <- get_models_list(model.permutations.list, 1)

# training set performance
models.metrics <- models.list %>% get_model_metrics %T>% print

# set color palettes (default = "Set1")
# models.list %>% get_model_metrics(palette = "Accent")
models.list %>% get_model_metrics(palette = "Dark2")

# get model comparison
models.list %>% head(-2) %>% resamples %>% dotplot
# models.list %>% resamples %>% bwplot

########################################
## 4.2 Testing Set Performance
########################################
# RMSE for all models on testing set
models.list %>%
  get_model_metrics %>%
  .$RMSE.testing %>% kable(caption = "testing set performance: RMSE")

models.list %>%
  get_model_metrics %>%
  .$RMSE.all %>% kable(caption = "training vs. testing set performance: RMSE")

################################################################################
################################################################################
################################################################################
#
# SCRIBBLE
#
################################################################################
################################################################################

# ggplot(data = testing.set, aes(x = PERF.all)) + geom_bar()
# 
# training.set$PERF.all %>% summary
# testing.set$PERF.all %>% summary
# 
# # testing set performance with lm
# predictions.best <- predict(models.list$lm, testing.set, na.action = na.pass)
# rmse.best.testing <- sqrt(mean((predictions.best- testing.set[[target.label]])^2)) %>% print
# rmse.best.training <- models.lists.all[[3]] %>% .$lm %>% .$results %>% .$RMSE %>% print


# ################################################################################
# # Compare preProcess outside/inside train()
# ################################################################################
# ## Compare preProcess outside/inside train()
# # models.list.name <- "data/models.lists.3.PERF.all.rds"
# # models.lists.all <- models.list.name %>% readRDS
# # models.list <- models.lists.all[[3]]
# 
# if (models.list.name == "data/models.lists.3.PERF.all.rds") {
#   models.lists.all <- "data/models.lists.3.PERF.all.rds" %>% readRDS
#   
#   ## models 1: preProcess outside train() 10-fold cv
#   models.lists.all[[1]]$result
#   
#   ## models 2: preProcess inside train(), 10-fold cv
#   models.lists.all[[2]]$result
#   
#   ## models 3: preProcess inside train(), 10-fold repeated cv
#   models.lists.all[[3]]$result
#   
# }

################################################################################
# RMSE > MSE
################################################################################
# RMSE
models.RMSE <- models.resamples %>% summary %>% .$statistics %>% .$RMSE %>% as.data.frame

# ## RMSE - conserve model names in rownames
# models.RMSE %>%
#   # safer than subset(select = Mean)
#   select(Mean) # only retains row names if no further action

# MSE
## MSE - add column rowname for model names + sort
models.RMSE %>% 
  tibble::rownames_to_column() %>%
  mutate(model=rowname, RMSE=Mean, MSE = Mean^2) %>% 
  select(model, RMSE, MSE) %>% 
  arrange(RMSE)

# ### alternative(RMSE - add column rowname for model names + sort)
# models.RMSE %>% 
#   mutate(RMSE=Mean, MSE = Mean^2) %>% 
#   select(RMSE, MSE) %>% 
#   arrange(RMSE) %>% 
#   # retaining the row names requires matrix transformation
#   as.matrix %>%
#   matrix(nrow = nrow(models.RMSE), # ncol = 2,
#          dimnames = list(dimnames(models.RMSE)[[1]], c("RMSE", "MSE")))

## MSE
### apply a function (square) to all elements of a dataframe
models.MSE <- models.RMSE %>%
  # square the RMSE
  sapply(function(x) x^2) %>% 
  ## tricky: preserve dataframe dimensions (matrix) AND model names (dimnames)
  matrix(nrow = nrow(models.RMSE), dimnames = dimnames(models.RMSE)) %>% 
  as.data.frame %T>% print

# MSE boxplots - base r
models.MSE %>% select(-`NA's`) %>% t %>% boxplot

# MSE boxplots - ggplot
models.MSE %>% select(-`NA's`) %>% t %>% as.data.frame %>% 
  tidyr::gather(key = model, value = MSE) %>% 
  ggplot(aes(x = reorder(model, desc(MSE), median), y = MSE)) +
  geom_boxplot() +
  # coord_flip() +
  xlab("model")



