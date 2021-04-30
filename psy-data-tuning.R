
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


################################################################################
######################

create_model_label <- function(model_permutation) {

  paste0(c("data/models.list",
           model_permutation,
           "100repeats.noimpute.rds"),
         collapse = ".")
}

create_tuning_label <- function(model_permutation, ..., suffix = "rds") {

  dots <- list(...) %>% discard(is.null)
  paste0(c("tuning/tuned",
           model_permutation,
           dots,
           suffix),
         collapse = ".")
}

#####################################################
# extract models selected for publishing
#####################################################
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

#####################################################
# get best model label for model tuning
#####################################################
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

#####################################################
# extract best model for model tuning
#####################################################
extract_best_model <- function(model_label, metric = "R") {

  models.published <- extract_models_published(model_label)

  model.metrics <- models.published %>%
    get_model_metrics(colors = color.scheme)

  best.model.label <- get_best_model(model.metrics, metric = metric)

  best.model <- models.published %>% pluck(best.model.label)

  return(best.model)
}

#####################################################
# create list of best models per benchmark
#####################################################
system.time(
  best.model.list <- model.permutations.string %>%
    map(~ create_model_label(.x) %>%
          extract_best_model("R")
    ) %>%
    set_names(model.permutations.string)
) # 17.3s

best.model.list %>% names

# BEST MODELS
map_chr(best.model.list, ~ .x$method)

#####################################################
# tune model
#####################################################
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

#####################################################
# get best multiple R from cross-validation folds
#####################################################
get_best_R <- function(model, digits = 4) {
  model %>%
    getTrainPerf() %>%
    .$TrainRsquared %>%
    sqrt() %>%
    round(., digits = digits)
}


model.permutations.string

model.label <- "PERF10.big5composites.all"
# model.label <- "PERF10.big5composites.R&D" # lm not tunable
model.label <- "PERF10.big5composites.sales"
model.label <- "PERF10.big5composites.support"
model.label <- "PERF10.big5items.all"
model.label <- "PERF10.big5items.R&D"
model.label <- "PERF10.big5items.sales"
model.label <- "PERF10.big5items.support"

best.model.R <- best.model.list %>% pluck(model.label)

# best.model.R
# best.model.R$method
# best.model.R$results
# best.model.R$bestTune
# best.model.R %>% getTrainPerf()
# best.model.R %>% plot
# modelLookup(best.model.R$method)
# best.model.R %>% plot

################################################################################
# model tuning of best model
################################################################################
# svmRadial: set tuning parameters
tune.parameters <- expand.grid(
  # .sigma = c(0.01, 0.05, 0.1, 0.2, 0.3),
  # .C = c(0.1, 0.2, 0.3, 0.4)
  .sigma = c(0.01, 0.02, 0.03),
  .C = c(0.3, 0.5, 1.0, 1.5)
)

# gbm: set tuning parameters
tune.parameters <- expand.grid(
  .shrinkage = c(0.05, 0.1, 0.15), # default 0.1
  .interaction.depth = c(1), # 1 always better than 2 = "Max Tree Depth
  .n.minobsinnode = c(5, 10, 15, 20),
  .n.trees = c(20, 50, 75, 100)
)

# NEW <- TRUE
NEW <- FALSE

#####################################################
# create | get tuned model
#####################################################
if (NEW) {

  model.tuned <- tune_model(best.model.R, tune.parameters, repeats = 10)
  R.tuned <- get_best_R(model.tuned) %>% gsub("0.", "R.", .) %>% print
  model.tuned %>% saveRDS(create_tuning_label(model.label))

} else {
  model.tuned <- readRDS(create_tuning_label(model.label))
  R.tuned <- get_best_R(model.tuned) %>% gsub("^0.", "R.", .) %>% print
}

#####################################################
# plot parameters of tuned model
#####################################################
dev.off()
model.tuned %>% plot
# savePlot(filename = create_tuning_label(model.label, suffix = "png"), type = "png")
dev.copy(png, create_tuning_label(model.label, R.tuned, suffix = "png"),
         width = 6, height = 4, units = "in", res = 100
)
dev.off()

# model.tuned1 <- model.tuned
model.tuned

best.model.R$bestTune
model.tuned$bestTune

best.model.R %>% get_best_R()
model.tuned %>% get_best_R()

best.model.R %>% plot
model.tuned %>% plot



