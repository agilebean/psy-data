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
  , "DALEX"
  , "furrr"
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
    str_detect("^RF|GBM|LR$") %>%
    # select specific list elements by name
    purrr::keep(models_list, .)
}

################################################################################
# MAIN: all models
################################################################################
# step1)
# read models.lists from all datasets

# NEW <- TRUE
NEW <- FALSE

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
  ) # 6.5s
}

datasets.models.list %>% names
models.list <- datasets.models.list %>% pluck(1) # composites
models.list <- datasets.models.list %>% pluck(5) # items

getOption("parallelly.fork.enable")
options(parallelly.fork.enable = TRUE)
plan(multicore)

imap(models.list,
     ~ paste(.y, .x$method))

get_permutation_fi_for_models_list <- function(models_list) {

  fi.list.DALEX <- models_list %>%

    imap(function(model_object, model_name) {

      print(paste("*********", model_object$method))

      training.set <- model_object$trainingData %>%
        select(.outcome, everything())

      target <- training.set$.outcome
      print(paste("***target"))

      features <- training.set %>% select(-.outcome)

      DALEX.explainer <- DALEX::explain(
        model = model_object,
        data = features,
        y = target,
        label = model_name, # .y in imap() is list element name
        colorize = TRUE
      )
      print("*** DALEX.explainer")

      set.seed(SEED)
      system.time(
        DALEX.permutation.fi <- model_parts(
          explainer = DALEX.explainer,
          loss_function = loss_root_mean_square,
          B = 50,
          type = "ratio"
        )
      ) # 78-84s for rf

      print("*** DALEX.permutation.fi")

      # bar_width = 5.9 items / 12.4 composites
      ( bar.width <- 20 / log(ncol(features)) )

      DALEX.permutation.fi.plot <- DALEX.permutation.fi %>%
        plot(
          bar_width = bar.width
          , title = "Permutation Feature importance"
          , subtitle = ""
        ) +
        scale_y_discrete(expand = c(0, 0)) # reduce space to axis
      DALEX.permutation.fi.plot
      print("*** DALEX.permutation.fi.plot")

      return(
        list(
          DALEX.explainer = DALEX.explainer
          , DALEX.permutation.fi = DALEX.permutation.fi
          , DALEX.permutation.fi.plot = DALEX.permutation.fi.plot
        )
      )
    })
  # 44s B=10, 220s B=50, 408s B=100

  return(fi.list.DALEX)
}

datasets.models.list %>% names

if (NEW) {
  system.time(
    datasets.permutation.fi.lists <- datasets.models.list %>%
      map(~ get_permutation_fi_for_models_list(.x))
  ) # 474s = 7.9min

  system.time(
    datasets.permutation.fi.lists %>%
      saveRDS(permutation.fi.lists.label)
  ) # 71s

} else {

  system.time(
    datasets.permutation.fi.lists <-
      readRDS(permutation.fi.lists.label)
  ) # 13.5s

}

datasets.permutation.fi.lists

fi.list <- datasets.permutation.fi.lists$PERF10.big5composites.all
fi.list <- datasets.permutation.fi.lists$PERF10.big5items.all

fi.list$LR$DALEX.permutation.fi.plot
fi.list$kNN$DALEX.permutation.fi.plot
fi.list$GBM$DALEX.permutation.fi.plot
fi.list$RF$DALEX.permutation.fi.plot
fi.list$SVM$DALEX.permutation.fi.plot

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
    axis_label = "facet"

  } else if (dataset_type == "composites") {

    list.of.lists <-
      get_listelements_by_string(models.varimp.list, "composites")
    width = 7
    height = 3
    axis_limit = 103 # composites
    axis_label = "factor"
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
                  text_labels = TRUE,
                  axis_label = axis_label,
                  axis_tick_labels = data.labels.long,
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


# change axis labels for factors/facets
data.labels.long
varimp.list$PERF10.big5items.all$GBM$importance.plot +
  scale_x_discrete(labels = data.labels.long)

get_listelements_by_string(models.varimp.list, "composites")
get_listelements_by_string(models.varimp.list, "items")

plot_varImp_list_of_lists("composites")
plot_varImp_list_of_lists("items")








