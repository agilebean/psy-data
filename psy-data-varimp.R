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

 # models_list <- models.list
  models_list %>%
    names %>%
    # tricky: avoid glmnet by squeezing ^lm$
    # str_detect("^ranger|rf|gbm|lm$") %>%
    str_detect("^rf|gbm|lm$") %>%
    # select specific list elements by name
    purrr::keep(models_list, .)
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
model.index = 5
data.labels <- model.permutations.labels[model.index,] %>%
  unlist() %>% as.vector() %T>% print

# 2) get data
models.list <- read_models_list(data.labels) %>% print
models.varimp <- models.list %>% get_models_varimp()
models.varimp$gbm %>% varImp()
models.varimp$rf %>% varImp()

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
models.varimp$gbm %>%
  print_correlation_table_from_model(digits = 2)

# 4) visualize feature importance
system.time(
  varimp.list <- create_plots_feature_importance(
    models.varimp, data.labels,
    # save = TRUE,
    axis_limit = 25.5)
)

models.varimp$lm %>% .$finalModel %>% summary()
varimp.list$lm
varimp.list$gbm
varimp.list$rf

models.varimp$gbm %>% varImp()

# problem:
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

#########################################
# create correlation matrices
system.time(
  correlation.list <-
    map(datasets.models.list,
        ~ .x %>%  # tricky: start with .x
          pluck("RF") %>%
          print_correlation_table_from_model(digits = 2)
    ) %>%
    set_names(model.permutations.strings)
) # 0.64s

correlation.list$PERF10.big5composites.all

map2(correlation.list, names(correlation.list),
       function(correlation_result, jobtype_label) {

         correlation_result$html.table %>%
           cat(., file = paste0(
             c("tables/corrtable", features.set.labels.list,
               jobtype_label, correlation_result$method, "html"),
             collapse = "."))
  })

#########################################
# create feature importance plots

# get gbm from each models.list
GBM.list <- datasets.models.list %>%
  map( ~ .x %>%  # tricky: start with .x
         pluck("GBM")
         create_plots_feature_importance(
           ., model.permutations.strings,
           # save = TRUE,
           # width = 7, height = 3, axis_limit = 103 # composites
           width = 6, height = 6, axis_limit = 26 # items
         )

         )

GBM.list$PERF10.big5composites.all %>%
  create_plots_feature_importance(
    ., "lalala",
    # save = TRUE,
    # width = 7, height = 3, axis_limit = 103 # composites
    width = 6, height = 6, axis_limit = 26 # items
  )

system.time(
  varimp.list <-
    map(datasets.models.list,
        ~ .x %>%
          pluck("GBM") %T>% print
    )
)

models.list <- datasets.models.list$PERF10.big5composites.all
varimp.list$PERF10.big5composites.all

system.time(
  result.list <-
    map(data.labels.list,
        function(data_labels) {
          read_models_list(data_labels) %>%
          get_models_varimp() %>%
            create_plots_feature_importance(
              ., data_labels,
              save = TRUE,
              # width = 7, height = 3, axis_limit = 103 # composites
              width = 6, height = 6, axis_limit = 26 # items
              )
        }
    )
)

system.time(
  ci.list <-
    map(data.labels.list,
        function(data_labels) {
          read_models_list(data_labels) %>%
          list_modify(target.label = NULL, testing.set = NULL) %>%
            list_modify(
              glmnet = NULL,
              kknn = NULL,
              xgbTree = NULL,
              xgbLinear = NULL,
              svmLinear = NULL,
              ranger = NULL) %>%
            map_dfr(.,
                    ~ calculate_ci_bootstrapped(.x, 1000),
                    .id = "model")
        }
    )
)

kable_table <- function(data, digits = 4,data_label) {
  data %>%
  knitr::kable(digits = digits, format = "html") %>%
    cat(file = paste0("tables/ci ", data_label, ".html"))
}
options(digits = 4)
ci.list$all %>% kable_table(data_label = "PERF10.items.all")
ci.list$`R&D` %>% kable_table(data_label = "PERF10.items.R&D")
ci.list$sales %>% kable_table(data_label = "PERF10.items.sales")
ci.list$support %>% kable_table(data_label = "PERF10.items.support")

ci.list$all %>% kable_table(data_label = "PERF10.items.all")
ci.list$`R&D` %>% kable_table(data_label = "PERF10.items.R&D")
ci.list$sales %>% kable_table(data_label = "PERF10.items.sales")
ci.list$support %>% kable_table(data_label = "PERF10.items.support")



#########################################
# create feature importance tables
varimp.list <- map(data.labels.list,
    ~read_models_list(.x) %>%
      get_models_varimp() %>%
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


