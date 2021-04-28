
libraries <- c("dplyr", "magrittr", "tidyverse"
               , "sjlabelled" # read SPSS
               , "caret", "doParallel"
               , "DataExplorer", "RColorBrewer"
               , "machinelearningtools"
               , "knitr", "pander"
)
# devtools::install_github("agilebean/machinelearningtools")
# unloadNamespace("machinelearningtools")
sapply(libraries, require, character.only = TRUE)

source("_labels.R")

data.label <- "data/models.list.PERF10.big5composites.all.100repeats.noimpute.rds"
# data.label <- "data/models.list.PERF10.big5items.all.100repeats.noimpute.rds"
# don't confuse: models.list.PERF10.big5composites.80-20.100repeats.noimpute

model.permutations.string

models.list <- data.label %>% readRDS() %>% print
models.list$knn$trainingData %>% dim()



################################################################################
######################

create_model_label <- function(model_permutation) {

  paste0(c("data/models.list",
           model_permutation,
           "100repeats.noimpute.rds"),
         collapse = ".")
}

extract_models_published <- function(model_label) {

  models.list <- readRDS(model_label)

  models.published <- models.list %>%
    list_modify(
      glmnet = NULL,
      kknn = NULL,
      xgbTree = NULL,
      xgbLinear = NULL,
      svmLinear = NULL,
      ranger = NULL) %>%
    list_modify(target.label = NULL, testing.set = NULL) %>%
    set_names(models.labels.published)

  return(models.published)
}

get_best_model <- function(model_metrics, metric = "R") {

  metric_sym <- rlang::sym(paste0(metric, ".mean"))

  best.model.label <- model_metrics %>%
    .$benchmark.all %>%
    {
      if (metric == "RMSE") {
        filter(., !!metric_sym == min(!!metric_sym))
      } else {
        filter(., !!metric_sym == max(!!metric_sym))
      }
    } %>%
    .$model

  return(best.model.label)
}

extract_best_model <- function(model_label, metric = "R") {

  models.published <- extract_models_published(model_label)

  model.metrics <- models.published %>%
    get_model_metrics(colors = color.scheme)

  best.model.label <- get_best_model(model.metrics, metric = metric)

  best.model.R <- models.published %>% pluck(best.model.label)

  return(best.model)
}

model.label <- create_model_label("PERF10.big5composites.R&D")
best.model.R <- extract_best_model(model.label)

##############################################
# create model metrics
##############################################

##############################################
# select best model for model tuning
##############################################

system.time(
  # create model.metrics for all models
  best.model.list <- model.permutations.string %>%
    map(~ create_model_label(.x) %>%
          extract_best_model("R")
    ) %>%
    set_names(model.permutations.string)
) # 15.3s

best.model.list %>% names

map_chr(best.model.list, ~ .x$method)


# model.label <- "PERF10.big5composites.all"
# model.label <- "PERF10.big5composites.R&D"
model.label <- "PERF10.big5composites.sales"
# model.label <- "PERF10.big5composites.support"
# model.label <- "PERF10.big5items.all"
# model.label <- "PERF10.big5items.R&D"
# model.label <- "PERF10.big5items.sales"
# model.label <- "PERF10.big5items.support"

best.model.R <- best.model.list %>% pluck(model.label)

##############################################
# model tuning of best model
##############################################
best.model.R
best.model.R$method
best.model.R$results
best.model.R$bestTune
best.model.R %>% getTrainPerf()
best.model.R %>% plot
# best.model.R$finalModel
modelLookup(best.model.R$method)
best.model.R %>% plot

# svmRadial: set tuning parameters
tune.parameters <- expand.grid(
  .sigma = c(0.01, 0.05, 0.1, 0.2, 0.3),
  .C = c(0.1, 0.2, 0.3, 0.4)
)

# gbm: set tuning parameters
tune.parameters <- expand.grid(
  .shrinkage = c( 0.05, 0.1, 0.15),
  .interaction.depth = c(1,2),
  .n.minobsinnode = c(5, 10, 15, 20),
  .n.trees = c(20, 50, 75, 100)
)

tune_model <- function(model, tune_grid, repeats = 10, seed = 171) {

  clus <- clusterOn()

  set.seed(seed)
  system.time(
    model.tuned <- caret::train(
      form = as.formula(".outcome ~ ."),
      data = model$trainingData,
      method = model$method,
      trControl = trainControl(method = "repeatedcv", number = 10,
                               repeats = repeats),
      tuneGrid = tune_grid
    )
  ) %T>% print
  clusterOff(clus)

  print(paste0("The lowest RMSE is: ",
               model.tuned %>% getTrainPerf %>% .$TrainRMSE %>% round(digits = 4)))
  print("The best tuning parameters are: ")
  print(model.tuned$bestTune)

  return(model.tuned)
}

model.tuned <- tune_model(best.model.R, tune.parameters, repeats = 10)

# model.tuned %>% saveRDS("tuning/PERF10.big5composites.all.tuned.rds")
model.tuned
model.tuned %>% plot
model.tuned$bestTune
model.tuned %>% getTrainPerf()


