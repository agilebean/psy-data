################################################################################
# Script:     psy-data-varimp
# Class:      Psychology Collaboration
# Topic:      Tenure and Job Performance
#
################################################################################
# load libraries
# detach("package:machinelearningtools", character.only = TRUE)
# devtools::install_github("agilebean/machinelearningtools", force = TRUE)
libraries <- c(
  "magrittr"
  , "caret"
  , "RColorBrewer"
  , "machinelearningtools"
  , "knitr"
  , "tidyverse"
)
sapply(libraries, require, character.only = TRUE)

source("_labels.R")

getOption("digits")

model.permutations.labels


################################################################################
# get data of models.list
################################################################################
get_data_models_list <- function(models_labels) {

  # get models label
  models.list.label <- output_filename(
    PREFIX, models_labels,
    paste0(CV.REPEATS, "repeats"), impute_method = IMPUTE.METHOD
  )

  # get model in model.permutations.labels by model index
  print(paste0("Reading file >> ", models.list.label))
  models.list <- readRDS(models.list.name)
  # models.list <- readRDS("data/models.list.PERF10.big5items.100repeats.noimpute.rds")

  models.varimp <- models.list %>%
    names %>%
    # tricky: avoid glmnet by squeezing ^lm$
    str_detect("^lm|gbm|rf$") %>%
    # select specific list elements by name
    purrr::keep(models.list, .)

  return(models.varimp)
}

################################################################################
# visualize feature importance
################################################################################
create_plots_feature_importance <- function(models_list, data_labels) {

  map2(
    models_list, names(models_list),
    function(model, model_label) {

      filename <- paste0(c("figures/importance",
                           data_labels, model_label, "png"),
                        collapse = ".") %>% print
      model %>%
        visualize_importance(relative = TRUE, labels = TRUE) %>%
        .$importance.plot %>%
        ggsave(
          filename = filename,
          plot = .,
          dpi = 450,
          width = 10,
          height = 10
        )
    })
}

################################################################################
# MAIN: single model
################################################################################
# 1) get config: from model.permutations.list by model index
model.index = 1
data.labels <- model.permutations.labels[model.index,] %>%
  unlist() %>% as.vector() %T>% print

# 2) get data
models.varimp <- get_data_models_list(data.labels)

# 3) correlation matrix
models.varimp$gbm %>%
  print_correlation_table_from_model(digits = 2)

# 4) visualize feature importance
system.time(
  varimp.list <- create_plots_feature_importance(models.varimp, data.labels)
)

# models.varimp %>% get_model_metrics()
varimp.list$gbm
varimp.list$rf
varimp.list$lm


################################################################################
# MAIN: all models
################################################################################
# 1) get config: from model.permutations.list by model index
data.labels.list <- model.permutations.labels %>%
  group_by(row_number()) %>%
  nest() %>%
  mutate(
    labels = map(data, ~ unlist(.x) %>% as.vector)
  ) %>%
  .$labels %>%
  set_names(model.permutations.labels$job_label)

system.time(
  result.list <-
    map(data.labels.list,
        function(data_labels) {
          get_data_models_list(data_labels) %>%
            create_plots_feature_importance(., data_labels)
        }
    )
)
result.list



