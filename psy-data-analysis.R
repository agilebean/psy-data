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

# select target and features
# target.label <- "PERF07"
target.label <- "PERF.all"

dataset.label <- paste0(c("data/dataset", target.label, "rds"), collapse = ".")

features.set <- "big5items"
# features.set <- "big5composite"

# devtools::install_github("agilebean/machinelearningtools", force = TRUE)
# load libraries
libraries <- c("dplyr", "magrittr", "tidyverse", "purrr"
               , "sjlabelled" # read SPSS
               , "caret", "doParallel"
               , "stargazer", "DataExplorer", "skimr"
               , "machinelearningtools"
               , "knitr", "pander"
)
sapply(libraries, require, character.only = TRUE)

# nominal <- FALSE # with ordinal as ORDERED factors
nominal <- TRUE # with ordinal as NOMINAL factor

seed <- 17

#######################################################################
# 1. Data Acquistion
#######################################################################
## START failed import methods ##
#
# file.raw <- rio::import(filename)
# file.raw <- spss.get(filename, use.value.labels = TRUE)
# file.raw <- foreign::read.spss(filename, to.data.frame = TRUE) %T>% glimpse
#
## END failed import methods ##

filename <- "data/Personality-Performance-Turnover-Chaehan So.sav"

file.raw <- sjlabelled::read_spss(filename, 
                                  atomic.to.fac = TRUE,
                                  verbose = FALSE) 

data.labels <- foreign::read.spss(filename) %>% 
  attributes %>% 
  .$variable.labels %T>% print

file.raw %<>%
  dplyr::select(-id, -prinum, -TESTDATE) %>% 
  tbl_df 

################################################################################
# 2: Data Preparation
################################################################################
if (nominal) {
  
  # data.raw <- sjlabelled::copy_labels(data.raw, df_origin = file.raw)
  # file.raw %>% str
  
  ## tricky: either mutate+one_of(column_labels) OR mutate_at+column_labels
  data.raw <- file.raw %>%
    # convert categorical variables to factors
    mutate_at((c("COMNAME", "team_id", "class", "job", "gender", "educa")),
              as.factor) %>%
    # convert numerical variables to numeric datatype
    mutate_at(vars(starts_with("TO")), as.numeric) %>% 
    # fix import error for PERF07
    mutate_at("PERF07", as.numeric) %T>% print
  # leave Big5 items' datatype to numeric bec. they are already converted
  
  # dataset %>% str
  # dataset %>% glimpse
  
} else { # factors treated as ordinal
  
}

################################################################################
# 2.1: Data Inspection
################################################################################

################################################################################
### 2.1.1 Data Structure
################################################################################
data.raw %>% glimpse
dataset %>% dim # n=1621
data.raw$LIFE_S_R %>% as.factor %>% levels
data.raw$JOB_S_R %>% as.factor %>% levels

################################################################################
### 2.1.2 Histograms
################################################################################
# data.raw %>% plot_histogram(nrow = 9, ncol = 6)

################################################################################
### 2.1.3 Missing Values
################################################################################
# visualize missing values
data.raw %>% DataExplorer::plot_missing() 

## find any rows containing NAs
dataset %>% filter_all(any_vars(is.na(.))) # n=1430

## find all rows without any NA = complete.cases
dataset %>% na.omit %>% dim # n=191
dataset %>% .[complete.cases(.),]

## find all NA-columns
## tricky: not intuitive
# dataset %>% select_if(~sum(is.na(.)) > 0) #  n=1621, cols=14
# dataset %>% Filter(function(x) !(all(x=="")), .) #  n=1621, cols=57

# find columms with most NAs
## count NAs per columns
na.columns <- data.raw %>% 
  # find any columns with NAs
  select_if(function(x) any(is.na(x))) %>% 
  # sum NAs for each column
  summarise_all(funs(sum(is.na(.)))) 

## rank top NA columns
na.sorted <- na.columns %>% 
  t %>% as.data.frame %>% # was matrix
  tibble::rownames_to_column() %>% 
  rename(NA.count = V1) %>% 
  arrange(desc(NA.count)) %T>% print

################################################################################
## 2.2 Data Cleaning
### Goal: Define a datasets with least # of NAs
### Set target = mean performance score ("PERF.all")
################################################################################

################################################################################
### 2.2.1 Prune dataset 
################################################################################
data.pruned <- data.raw %>% 
  # remove top NA columns
  select(-JOB_S_R, # 1027 NAs
         -howlong, #  910 NAs
         -PERF11,  #  502 NAs
         -PERF10,  #  431 NAs
         -job,     #  342 NAs
         # -PERF09,  #  331 NAs
         # -PERF07, # 309 NAs
         -TO07,    #  288 NAs
         -class,   #  272 NAs +(904-713)
         -team_id, #  238 NAs +(1053-904)
         # -PERF08,  #  198 NAs +(1202-1053)
         -LIFE_S_R#  107 NAs +(1288-1202)
  ) %>% 
  # create mean performance & turnover score
  mutate(PERF.all = rowMeans(select(., starts_with("PERF")) ) ) %>%
  mutate(TO.all = rowMeans(select(., starts_with("TO")) ) ) %T>% print 

################################################################################
### 2.2.2 Clean dataset
################################################################################
# final check NAs
dataset %>% filter_all(any_vars(is.na(.))) # n(NA)=333
data.pruned %>% nrow
## final removal of NA rows
dataset <- data.pruned %>% drop_na %T>% { print(nrow(.)) }

################################################################################
# 2-3: Select the target & features
################################################################################
dataset %>% names
dataset %>% dim

target.label
features <- dataset %>% 
  select(-target.label,
         -starts_with("PERF"),
         -starts_with("TO"),
         # -"LIFE_S_R", -"JOB_S_R"
  ) %>% 
  names %T>% print

################################################################################
# 2-3: Split the data
################################################################################
# shuffle data
set.seed(seed)
shuffle.index <- dataset %>% nrow %>% sample
dataset %<>% .[shuffle.index,] %T>% print
# short version:
# dataset %<>% nrow %>% sample %>% dataset[.,] %T>% print

# later: imputation of NAs
# dataset %>% preProcess(method="knnImpute") %>% print

# dataset subsetting for tibble: [[
training.index <- createDataPartition(dataset[[target.label]], p = .75, list = FALSE)
testing.set <- dataset[-training.index, ]
training.set <- dataset[training.index, ]

################################################################################
# PART 3: Train Model
# 3-1: Select a model
# 3-2: Select the target, features, training data
# 3-3: Train the model with the target and features
################################################################################
# 3-1: Select a model: 
algorithm.list <- c(
  "lm",
  "knn",
  "gbm",
  "rf",
  "ranger",
  "xgbTree",
  "xgbLinear",
  "svmLinear",
  "svmRadial"
)

formula1 <- features %>% 
  paste(collapse = " + ") %>% 
  paste(target.label, "~", .) %>% 
  as.formula %T>% print

models.list.name <- paste0(c("data/models.list", target.label, "rds"), 
                           collapse = ".") %T>% print

if (mode == "new") {
  
  cluster.new <- clusterOn()
  
  set.seed(seed)
  
  training_configuration <- trainControl(method = "repeatedcv",
                                         number = 10, repeats = 3)
  
  models.list <- list()
  
  system.time(
    models.list <- algorithm.list %>% 
      lapply(function(algorithm_label) {
        train(formula1
              , method = algorithm_label
              , data = training.set
              # apply preProcess within cross-validation folds
              , preProcess = c("center", "scale")
              , trControl = training_configuration
        )
      }) %>% 
      setNames(algorithm.list)
  )
  
} else if (mode == "old") {
  
  models.list <- readRDS(models.list.name)
  models.list %>% resamples %>% dotplot
  
}
stopCluster(cluster.new)

################################################################################
# Training Set Performance:
# list mean + sd for all model metrics
################################################################################
# training set performance
models.metrics <- models.list %>% get_model_metrics %T>% print
# models.list %>% saveRDS(models.list.name)

# set color palettes (default = "Set1")
# models.list %>% get_model_metrics(palette = "Set2")
models.list %>% get_model_metrics(palette = "Dark2")

# get model comparison
models.list %>% resamples %>% dotplot
# models.list %>% resamples %>% bwplot

##########################################################
## Performance - testingset
##########################################################
# RMSE for all models on testing set
models.list %>% get_model_metrics

models.list %>% get_model_metrics %>% .$Rsquared.training %>% kable

models.list %>% 
  get_model_metrics(target.label, testing.set) %>% 
  .$RMSE.testing

models.list %>% 
  get_model_metrics(target.label, testing.set) %>% 
  .$RMSE.all

################################################################################
################################################################################
################################################################################
#
# SCRIBBLE
#
################################################################################
################################################################################
ggplot(data = testing.set, aes(x = PERF.all)) +
  geom_bar()

training.set$PERF.all %>% summary
testing.set$PERF.all %>% summary

# testing set performance with lm
predictions.best <- predict(models.list$lm, testing.set, na.action = na.pass)
rmse.best.testing <- sqrt(mean((predictions.best- testing.set[[target.label]])^2)) %>% print
rmse.best.training <- models.lists.all[[3]] %>% .$lm %>% .$results %>% .$RMSE %>% print


################################################################################
# Compare preProcess outside/inside train()
################################################################################
## Compare preProcess outside/inside train()
# models.list.name <- "data/models.lists.3.PERF.all.rds"
# models.lists.all <- models.list.name %>% readRDS
# models.list <- models.lists.all[[3]]

if (models.list.name == "data/models.lists.3.PERF.all.rds") {
  models.lists.all <- "data/models.lists.3.PERF.all.rds" %>% readRDS
  
  ## models 1: preProcess outside train() 10-fold cv
  models.lists.all[[1]]$result
  
  ## models 2: preProcess inside train(), 10-fold cv
  models.lists.all[[2]]$result
  
  ## models 3: preProcess inside train(), 10-fold repeated cv
  models.lists.all[[3]]$result
  
}

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


