library(caret)
library(tidyverse)
library(magrittr)
library(mlbench)
data(BostonHousing)

devtools::install_github("agilebean/machinelearningtools", force = TRUE)
detach("package:machinelearningtools", character.only = TRUE)
library(machinelearningtools)

seed <- 171

# shuffled <- TRUE
shuffled <- FALSE

if (shuffled) {
  dataset <- BostonHousing %>% nrow %>% sample %>% BostonHousing[., ]
} else {
  dataset <- BostonHousing %>% as_tibble()
}

dataset <- iris

# target_label <- "medv" # BostonHousing
target_label <- "Species" # iris
features_labels <- dataset  %>%
  # select_if(is.numeric) %>%
  select(-target_label) %>% names %T>% print

# define ml algorithms to train
algorithm_list <- c(
  # "lm"
  # , "glmnet"
  "knn"
  , "gbm"
  , "rf"
  , "svmRadial"
)

# repeated cv
training_configuration <- trainControl(
  method = "repeatedcv", number = 10
  , repeats = 10
  , savePredictions = "final",
  # , returnResamp = "all"
)

# preprocess by standardization within each k-fold
preprocess_configuration = c("center", "scale")

# select variables
dataset %<>% select(target_label, features_labels) %>% na.omit

# dataset subsetting for tibble: [[
set.seed(seed)
training.index <- createDataPartition(dataset[[target_label]], p = 0.8, list = FALSE)
training.set <- dataset[training.index, ]
# if split_ratio == 100%, then create no testing.set
testing.set <- testing.set <- dataset[-training.index, ]

########################################
# 3.2: Select the target & features
########################################
target <- training.set[[target_label]]
features <- training.set %>% select(features_labels) %>% as.data.frame

########################################
# 3.3: Train the models
########################################
models.list <- list()

set.seed(171)
clus <- machinelearningtools::clusterOn()
models.list <- algorithm_list %>%

  map(function(algorithm_label) {
    model <- train(
      x = features,
      y = target,
      method = algorithm_label,
      preProcess = preprocess_configuration,
      trControl = training_configuration
    )
    return(model)
    }
  ) %>%
  setNames(algorithm_list)

machinelearningtools::clusterOff(clus)

if (shuffled) {
  rmse.shuffle <- models.list %>%
    get_testingset_performance(
      target_label = target_label,testing_set = testing.set
    )
} else {
  rmse.noshuffle <- models.list %>%
    get_testingset_performance(
      target_label = target_label,testing_set = testing.set
    )
}

rmse.shuffle
rmse.noshuffle

merge(rmse.noshuffle[-3], rmse.shuffle[-3], by = "model")


models.list %>% resamples %>% bwplot()
models.list %>% resamples %>% dotplot()
models.metrics <- models.list %>% get_model_metrics()
models.metrics$metric1.resamples.boxplots


################################################################################
# predict on testing set for Stackoverflow

observed <- testing.set[[target_label]]
models.list %>%
  predict(testing.set) %>%
  map_df(function(predicted) {
    sqrt(mean((observed - predicted)^2))
    }) %>%
  t %>% as_tibble(rownames = "model") %>%
  rename(RMSE.testing = V1) %>%
  arrange(RMSE.testing) %>%
  as.data.frame



################################################################################
`if`(c(T, F), 1:2, 3:4)models.list <- readRDS("data/models.list.PERF09.big5composites.2repeats.noimpute.rds")
models.list %>% get_model_metrics()
metrics <- models.list %>% get_model_metrics()
metrics

# 4. Evaluate Models
################################################################################

set.seed(seed)
index <- dataset %>% nrow %>% sample
# index %>% table %>% as_tibble() %>% filter(n > 1)
shuffled <- dataset[index,]

trainrows <- createDataPartition(shuffled[[target.label]], p = .8, list = FALSE)

train <- shuffled[trainrows,] %>% print
test <- shuffled[-trainrows,] %>% print

train$PERF09 %>% table %>% data.frame %>% mutate(prop = Freq/nrow(train)*100)
test$PERF09 %>% table %>% data.frame %>% mutate(prop = Freq/nrow(test)*100)

################################################################

target_label <- "PERF09"
features_set_label <- "big5composites"
features_labels <- dataset %>% get_features(target_label, features_set_label)

dataset %<>% select(target_label, features_labels) %>%
  # for non-imputed data, #NA can differ for different targets
  na.omit

set.seed(seed)
training.index <- createDataPartition(
  dataset[[target_label]], p = 0.8, list = FALSE)

training.set <- dataset[training.index, ] %T>% print
testing.set <- dataset[-training.index, ] %T>% print

training.set %>% select(LIFE_S_R, PERF09)
training.set$PERF09 %>% table %>% data.frame %>% mutate(prop = Freq/nrow(training.set)*100)
testing.set$PERF09 %>% table %>% data.frame %>% mutate(prop = Freq/nrow(testing.set)*100)


# http://pierreroudier.github.io/teaching/20171014-DSM-Masterclass-Hamilton/machine-learning-caret.html#stratified_random_sampling_(based_on_the_outcome)
idx_train <- clhs::clhs(dataset, size = 0.8*nrow(dataset),
                        iter = 5000,
                        progress = TRUE)

stratified <- dataset[idx_train,]
train <- stratified[trainrows,] %>% print
test <- stratified[-trainrows,] %>% print

train$PERF09 %>% hist
test$PERF09 %>% hist

train$PERF09 %>% table %>% data.frame %>% mutate(prop = Freq/nrow(train)*100)
test$PERF09 %>% table %>% data.frame %>% mutate(prop = Freq/nrow(test)*100)


