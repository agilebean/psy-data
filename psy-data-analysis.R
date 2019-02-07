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

# select target and features
# target.label <- "PERF.all"
# target.label <- "PERF09"

# features.set <- "big5items"
# features.set <- "big5composites"

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

if (mode == "report.single") {
  
  output.filename <- paste0(c("psy-data-analysis", 
                              target.label, features.set, "pdf"),
                            collapse = "-") %>% 
    gsub("-pdf", ".pdf", .) %>% print
  
  rmarkdown::render(input = "psy-data-analysis.Rmd",
                    params = list(target.label = target.label,
                                  target.label = features.set),
                    output_file = output.filename)
  
} else if (mode == "report.all") {
  
  target.label.list <- c("PERF09", "PERF.all")
  features.set.list <- c("big5items", "big5composites")
  
  input.list <- crossing(target_label = target.label.list, 
                         features_set = features.set.list)
  
  render_report <- function(target_label, features_set) {
    
    output.filename <- paste("psy-test", target_label, features_set, "pdf", sep = ".")
    
    rmarkdown::render(input = "psy-data-analysis.Rmd",
                      params = list(target.label = target_label,
                                    features.set = features_set),
                      output_file = output.filename)
  }
  
  input.list %>% pmap_chr(render_report)
}


# nominal <- FALSE # with ordinal as ORDERED factors
nominal <- TRUE # with ordinal as NOMINAL factor

seed <- 17

#######################################################################
# 1. Data Acquistion
#######################################################################
dataset <- readRDS(dataset.label) %T>% print

#######################################################################
# 2. Data Preparation
#######################################################################
########################################
## 2.1 Data Inspection
########################################
dataset %>% glimpse

########################################
## 2.2 Data Cleaning
########################################

# remove turnover items except "TO.all"
dataset %<>% select(-matches("^TO[0-9]{2}$"))  %T>% print

if (mode=="modify") {
  if (features.set == "big5items") {
    
    dataset %<>% 
      # remove composite scores - equivalent to (-nn, -ee, -oo, -aa, -cc)
      select(-matches("(oo|cc|ee|aa|nn)$")) 
    
  } else if (features.set == "big5composites") {
    
    dataset %<>% 
      # remove Big5 items
      select(-matches(".*(1|2|3|4|5|6)"))
  }
  
  dataset %>% print
  
  if (target.label == "PERF09") {
    
    dataset %<>% 
      # select(-matches("^PERF0[8|9]$")) %>% 
      select(-PERF.all, -TO.all) %T>% print
    
  } else if (target.label == "PERF.all") {
    
    dataset %<>% select(-matches("^PERF[0-9]{2}$"))
  }  
}



################################################################################
# 3. Train Model
# 3-1: Select a model
# 3-2: Select the target, features, training data
# 3-3: Train the model with the target and features
################################################################################

if (mode == "new") {
  
  cluster.new <- clusterOn()
  
  # set.seed(seed)
  set.seed(seed)
  
  training.configuration <- trainControl(method = "repeatedcv", 
                                         number = 10, repeats = 3)
  
  train_model_permutations <- function(target_label, features_set) {
    
    ########################################
    ## 2.3 Select the target & features
    ########################################
    target_label %>% print
    features_set %>% print
    
    ########################################
    ## 2.4 Split the data
    ########################################
    set.seed(seed)
    # shuffle data - short version:
    dataset %<>% nrow %>% sample %>% dataset[.,] %T>% print
    
    # later: imputation of NAs
    # dataset %>% preProcess(method="knnImpute") %>% print
    
    
    # dataset subsetting for tibble: [[
    set.seed(seed)
    training.index <- createDataPartition(dataset[[target_label]], p = .80, list = FALSE)
    testing.set <- dataset[-training.index, ]
    training.set <- dataset[training.index, ]
    
    # define models.list name
    models.list.name <- paste0(c("data/models.list", target_label, features_set, "rds"), 
                               collapse = ".") %T>% print
    # define features
    features <- dataset %>% 
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
    
    
    # define formula
    formula1 <- features %>% 
      paste(collapse = " + ") %>% 
      paste(target_label, "~", .) %>% 
      as.formula %T>% print
    
    models.list <- list()
    
    ########################################
    # 3-1: Select a model
    ########################################
    algorithm.list <- c(
      "lm"
      , "knn"
      , "gbm"
      , "rf"
      , "ranger"
      , "xgbTree"
      , "xgbLinear"
      , "svmLinear"
      , "svmRadial"
    )
    
    system.time(
      models.list <- algorithm.list %>% 
        map(function(algorithm_label) {
          train(formula1
                , method = algorithm_label
                , data = training.set[,]
                , preProcess = c("center", "scale")
                , trControl = training.configuration
          )
        }) %>% 
        setNames(algorithm.list)
    )
    
    models.list %>% saveRDS(models.list.name)
    models.list %>% resamples %>% dotplot
    
  }
  
  model.permutations.list %>% pmap(train_model_permutations)
  
} else if (mode == "old") {
  
  models.list <- readRDS(models.list.name)
  models.list %>% resamples %>% dotplot
  
}

stopCluster(cluster.new)

################################################################################
# 4. Evaluate Models
################################################################################

get_models_list <- function(permutation_list, model_index) {
  
  permutation <- permutation_list %>% map_df(model_index) %>% print
  
  models.list.name <- paste0(c("data/models.list", permutation$target_label, 
                               permutation$features_set, "rds"), 
                             collapse = ".") %T>% print
  
  models.list  <- readRDS(models.list.name)
  
  return(models.list)
}

########################################
## 4.1 Training Set Performance
########################################
mode <- "old"

target_label <- "PERF.all"
features_set  <-  "big5items"
features_set  <-  "big5composites"

features <- dataset %>% 
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


# get model
models.list <- get_models_list(model.permutations.list, 2)

# training set performance
models.metrics <- models.list %>% get_model_metrics %T>% print
if (mode == "new") { models.list %>% saveRDS(models.list.name) }

# set color palettes (default = "Set1")
# models.list %>% get_model_metrics(palette = "Set2")
models.list %>% get_model_metrics(palette = "Dark2")

# get model comparison
models.list %>% resamples %>% dotplot
# models.list %>% resamples %>% bwplot

########################################
## 4.2 Testing Set Performance
########################################

# RMSE for all models on testing set
models.list %>% get_model_metrics

models.list %>% get_model_metrics %>% .$Rsquared.training %>% kable


models.list %>%
  get_model_metrics(params$target.label, testing.set) %>%
  .$RMSE.testing %>% kable(caption = "testing set performance: RMSE")

models.list %>%
  get_model_metrics(params$target.label, testing.set) %>%
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



