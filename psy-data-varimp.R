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
  , "gbm"
)
sapply(libraries, require, character.only = TRUE)

source("_labels.R")
source("_common.R")

getOption("digits")


################################################################################
# get data of models.list
################################################################################
get_models_varimp <- function(models_list) {

  models_list %>%
    names %>%
    # tricky: avoid glmnet by squeezing ^lm$
    # str_detect("^ranger|rf|gbm|lm$") %>%
    # str_detect("^rf|gbm|lm$") %>%
    str_detect("^RF|GBM|LR$") %>%
    # select specific list elements by name
    purrr::keep(models_list, .)
}

################################################################################
# MAIN: single model
################################################################################
# 1) get config: from model.permutations.list by model index
model.index = 5
data.labels <- model.permutations.labels[model.index,] %>%
  unlist() %>% as.vector() %T>% print

# 2) get data
models.list <- read_models_list(data.labels) %>% print
models.varimp <- models.list %>% get_models_varimp()
library(gbm)
models.varimp$GBM %>% varImp()
models.varimp$RF %>% varImp()

# # doesn't work!
# models.varimp$svmRadial %>% varImp()
# models.varimp$svmRadial %>% varImp(useModel = FALSE, nonpara = FALSE)
# models.varimp$svmRadial %>% varImp(useModel = FALSE)
# models.varimp$svmRadial %>% varImp(nonpara = FALSE)
# models.varimp$svmRadial %>% varImp(scale = FALSE)
#
# models.varimp$svmRadial$finalModel %>% varImp()
# models.varimp$svmRadial$finalModel %>% class
# methods(varImp)
#
# rminer::Importance(M = models.varimp$svmRadial$finalModel,
#                    data = models.varimp$svmRadial$trainingData,
#                    method = "sens")

# 3) correlation matrix
models.varimp$GBM %>%
  print_correlation_table_from_model(digits = 2)

# 4) visualize feature importance
system.time(
  plot.fi <- create_feature_importance_plot(
    models.varimp$GBM, "gbm", data_labels = data.labels,
    # save = TRUE,
    axis_limit = 25.5)
)
plot.fi

models.varimp$LR %>% .$finalModel %>% summary()
models.varimp$GBM %>% varImp()

# problem ranger:
# for varImp(), ranger needs explicit importance = "impurity" argument
# -> implemented this in benchmark_algorithms on July 12, 2020:
# https://github.com/agilebean/machinelearningtools/commit/427e0d5


################################################################################
# MAIN: all models
################################################################################
# step1)
# read models.lists from all datasets

# NEW <- TRUE
NEW <- FALSE

data.label.all <- "data/models.list.PERF10.ALL.rds"

if (NEW) {
  system.time(
    datasets.models.list <- model.permutations.strings %>%
      map(~ read_models_list(.x)) %>%
      set_names(model.permutations.strings)
  ) # 6.8s

  system.time(
    datasets.models.list %>% saveRDS(data.label.all)
  ) # 31s

} else {
  system.time(
    datasets.models.list <- readRDS(data.label.all)
  ) # 5.3s
}

datasets.models.list

#####################################################
# create correlation matrices
#####################################################
# step1: create correlation tables (html + data)
correlation.list <-
  map(datasets.models.list,
      ~ .x %>%  # tricky: start with .x
        pluck("RF") %>%
        print_correlation_table_from_model(digits = 2)
  ) %>%
  set_names(model.permutations.strings)
# 0.68s

correlation.list$PERF10.big5composites.all

# step2: save correlation html tables
map2(correlation.list, names(correlation.list),
       function(correlation_result, jobtype_label) {

         correlation_result$html.table %>%
           cat(., file = paste0(
             c("tables/corrtable", features.set.labels.list,
               jobtype_label, correlation_result$method, "html"),
             collapse = "."))
  })

#####################################################
# create feature importance tables & plots
#####################################################
# step1: extract varImp-able models from each models.list
models.varimp.list <- datasets.models.list %>%
  # tricky: start with .x
  map( ~ .x %>% get_models_varimp() )

# select model label
model.label.publish <- "GBM"

# # try single model
# models.varimp.list$PERF10.big5composites.all %>%
#   pluck(model.label.publish) %>%
#   visualize_importance(relative = TRUE, labels = TRUE)

# step2: create varimp plots+tables for each models.list
varimp.list <- models.varimp.list %>%
  map(~ .x %>% # .x = models.list
        map( ~ visualize_importance(
          .x, relative = TRUE, labels = TRUE
        )))

# get varImp plots - only for model.label.publish
varimp.list %>% map(
  ~ .x %>%
    pluck(model.label.publish) %>%
    pluck("importance.plot") )

# for BigFive Factors (oo, cc, ee, aa, nn)
# options(digits = 3)
varimp.list$PERF10.big5composites.all$GBM
varimp.list$`PERF10.big5composites.R&D`$GBM
varimp.list$PERF10.big5composites.sales$GBM
varimp.list$PERF10.big5composites.support$GBM

# for BigFive Facets (oo1~5, cc1~5, ee1~5, aa1~5, nn1~5)
# options(digits = 2)
varimp.list$PERF10.big5items.all$GBM
varimp.list$`PERF10.big5items.R&D`$GBM
varimp.list$PERF10.big5items.sales$GBM
varimp.list$PERF10.big5items.support$GBM

# step3: save varImp plots
# # this works - but does not consider axis_limits
# map2(
#   varimp.list, names(varimp.list),
#   ~ .x %>%
#     pluck(model.label.publish) %>%
#     pluck("importance.plot") %>%
#     ggsave(
#       filename =  paste0(c("figures/importance",
#                            .y, model.label.publish, "png"),
#                          collapse = "."),
#       plot = .,
#       dpi = 300,
#       width = 7, height = 3
#     )
# )

# save varImp plots with different axis_limits for composites/items
plot_varImp_list_of_lists <- function(dataset_type) {

  if (dataset_type == "items") {

    list.of.lists <-
      get_listelements_by_string(models.varimp.list, "items")
    width = 6
    height = 6
    axis_limit = 48 # items

  } else if (dataset_type == "composites") {

    list.of.lists <-
      get_listelements_by_string(models.varimp.list, "composites")
    width = 7
    height = 3
    axis_limit = 103 # composites
  }

  map2(list.of.lists, names(list.of.lists),

       function(models_list, models_list_label) {

         map2(models_list, names(models_list),

              function(model_object, model_label) {

                filename <- paste0(
                  c("figures/importance",
                    models_list_label, model_label, "png"),
                  collapse = ".") %>% print

                visualize_importance(
                  model_object,
                  relative = TRUE,
                  labels = TRUE,
                  save_label = filename,
                  width = width,
                  height = height,
                  axis_limit = axis_limit
                )
              }
         )
       }
  )

}


get_listelements_by_string(models.varimp.list, "composites")
get_listelements_by_string(models.varimp.list, "items")

plot_varImp_list_of_lists("composites")
plot_varImp_list_of_lists("items")








