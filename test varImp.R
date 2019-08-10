# devtools::install_github("agilebean/machinelearningtools", force = TRUE)
# load libraries
libraries <- c("dplyr", "magrittr", "tidyverse"
               , "sjlabelled" # read SPSS
               , "caret", "doParallel"
               , "DataExplorer", "RColorBrewer"
               , "machinelearningtools"
               , "knitr", "pander"
)
sapply(libraries, require, character.only = TRUE)

seed <- 17

params <- list()
params$target.label <- "PERF.all"
# params$target.label <- "PERF09"
# params$features.set <- "big5items"
params$features.set <- "big5composites"
params$split.ratio <- 0.80
params$cv.repeats <- 100
# params$cv.repeats <- 10
# params$impute.method <- "noimpute"
params$impute.method <- "knnImpute"

models.list.name <- paste0(c("data/models.list",
                             params$target.label,
                             params$features.set,
                             paste0(params$cv.repeats, "repeats"),
                             params$impute.method,
                             "rds"),
                           collapse = ".") %T>% print

models.list <- models.list.name %>% readRDS

# get target label + testing set and then remove from models.list
target.label <- models.list$target.label
testing.set <- models.list$testing.set
models.list %<>% purrr::list_modify(target.label = NULL, testing.set = NULL)

########################################
# get model metrics
# models.metrics <- models.list %>% get_model_metrics

# display RMSE trainingset performance - table
# models.metrics$metric1.resamples.boxplots
########################################

models.list %>% map(1)
models.varimp <- models.list %>% head(-5) %>% list_modify(knn = NULL)
models.varimp %>% names


# only rf & doesn't work if not set importance=TRUE
## tricky: varImp/varImpPlot do NOT work on .$finalModel!
models.list$rf %>% varImp()
# models.list$rf$finalModel %>% varImp()

visualize_importance <- function (importance_object) {

  if (class(importance_object) == "varImp.train") {
    importance_object %<>% .$importance
  }
  if (!hasName(importance_object, "rowname")) {
    importance_object %<>% rownames_to_column()
  }

  importance_object %>%
    setNames(c("variable", "Importance")) %>%
    ggplot(data = ., aes(x = reorder(variable, Importance), y = Importance)) +
    theme_minimal() +
    geom_bar(stat = "identity", fill = "#114151") +
    coord_flip() +
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 12)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 102)) +
    xlab("item") + ylab("variable importance")
}

varimp.plots <- models.varimp %>%
  map(function(model) {
    require(gbm)
    model %>% varImp %>% visualize_importance()
  })

varimp.plots$lm
varimp.plots$glm
varimp.plots$gbm
varimp.plots$rf

######################################################################
# visualize variable importance - api
######################################################################

## tricky: $importance is created for rf even without train(importance = TRUE)
## tricky: must declare train(importance = TRUE) parameter!

##
# works
models.list$lm %>% varImp()
models.list$glm %>% varImp()
library(gbm)
models.list$gbm %>% varImp()

# alternative methods to get varImp from randomForest object
models.list$rf$finalModel %>% list_variable_importance()
models.list$rf$finalModel %>% randomForest::varImpPlot()
models.list$rf$finalModel %>% visualize_variable_importance_rf

######################################################################
# Alternative visualization - manual
######################################################################
# plot variable importance for randomForest
result <- models.list$rf$finalModel
result %>% randomForest::varImpPlot()
result$importance %>% as.data.frame %>%
  tibble::rownames_to_column() %>%
  mutate(Importance = round(IncNodePurity * 100/max(IncNodePurity), digits =2)) %>%
  arrange(-IncNodePurity)

# plot variable importance for caret::train("rf") or caret::train("gbm")
varImp(result)$importance %>%
  tibble::rownames_to_column() %>%
  arrange(-Overall) %>%
  ggplot(data = ., aes(x = reorder(rowname, Overall), y = Overall)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("item") + ylab("variable importance")
