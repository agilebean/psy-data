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
  models.list <- readRDS(models.list.label)
  # models.list <- readRDS("data/models.list.PERF10.big5items.all.100repeats.noimpute.rds")

  models.varimp <- models.list %>%
    names %>%
    # tricky: avoid glmnet by squeezing ^lm$
    # str_detect("^ranger|rf|gbm|lm$") %>%
    str_detect("^rf|gbm|lm$") %>%
    # select specific list elements by name
    purrr::keep(models.list, .)

  return(models.varimp)
}


################################################################################
# visualize feature importance
################################################################################
create_plots_feature_importance <- function(
  models_list, data_labels, save = FALSE,
  width = 10, height = 10, axis_limit = NULL) {

  map2(
    models_list, names(models_list),
    function(model, model_label) {

      filename <- paste0(c("figures/importance",
                           data_labels, model_label, "png"),
                        collapse = ".") %>% print
      model %>%
        visualize_importance(relative = TRUE, labels = TRUE,
                             axis_limit = axis_limit) %T>%
        { print(.$importance.table) } %>%
        .$importance.plot %>%
        {
          if (save) {
            ggsave(
              filename = filename,
              plot = .,
              dpi = 450,
              width = width,
              height = height
            )
          } else {
            .
          }
        }
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
  varimp.list <- create_plots_feature_importance(
    models.varimp, data.labels,
    # save = TRUE,
    axis_limit = 25.5)
)



varimp.list$gbm
models.varimp$gbm %>% varImp()
# problem:
# for varImp(), ranger needs explicit importance = "impurity" argument
# -> implemented this in benchmark_algorithms on July 12, 2020:
# https://github.com/agilebean/machinelearningtools/commit/427e0d5


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

#########################################
# create correlation matrices
system.time(
  correlation.list <-
    map(data.labels.list,
        function(data_labels) {
          get_data_models_list(data_labels) %>%
            purrr::keep(names(.) %in% c("rf")) %>%
            map(.,
              ~ print_correlation_table_from_model(.x, digits = 2)
            )
        })
)

correlation.list$all$rf$html.table

map2(correlation.list, names(correlation.list),
       function(jobtype_result, jobtype_label) {

         model_object <- jobtype_result$rf
         model_object$html.table %>%
           cat(., file = paste0(
             c("tables/corrtable", features.set.labels.list,
               jobtype_label, model_object$method, "html"),
             collapse = "."))
  })

#########################################
# create feature importance plots
system.time(
  result.list <-
    map(data.labels.list,
        function(data_labels) {
          get_data_models_list(data_labels) %>%
            create_plots_feature_importance(
              ., data_labels,
              save = TRUE,
              # width = 7, height = 3, axis_limit = 103 # composites
              width = 6, height = 6, axis_limit = 26 # items
              )
        }
    )
)


#########################################
# create feature importance tables
varimp.list <- map(data.labels.list,
    ~get_data_models_list(.x) %>%
      map(., ~.x %>% visualize_importance(relative = TRUE, labels = TRUE))
      )

# for BigFive Factors (oo, cc, ee, aa, nn)
# options(digits = 3)
varimp.list$`R&D`$lm$importance.table
varimp.list$sales$gbm$importance.table
varimp.list$support$gbm$importance.table
varimp.list$all$gbm$importance.table

# for BigFive Facets (oo1~5, cc1~5, ee1~5, aa1~5, nn1~5)
# options(digits = 2)
varimp.list$`R&D`$rf$importance.table
varimp.list$sales$rf$importance.table
varimp.list$support$rf$importance.table
varimp.list$all$rf$importance.table

varimp.list %>% map(~ .x %>% .$importance.table)


