################################################################################
# Script:     psy-data-varimp
# Class:      Psychology Collaboration
# Topic:      Tenure and Job Performance
#
################################################################################
# load libraries
# detach("package:machinelearningtools", character.only = TRUE)
# devtools::install_github("agilebean/machinelearningtools", force = TRUE)
# library(credentials)
# set_github_pat(force_new = FALSE, validate = interactive(), verbose = TRUE)

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
# get permutation feature importance (pimp) for models.list
################################################################################
get_pimp_for_models_list <- function(
  models_list, no_permutations = 50, seed = 171) {



  fi.list.DALEX <- models_list %>%

    imap(function(model_object, model_name) {

      # model_object <- models.list$SVM
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

      set.seed(seed)
      system.time(
        DALEX.permutation.fi <- model_parts(
          explainer = DALEX.explainer,
          loss_function = loss_root_mean_square,
          B = no_permutations,
          type = "ratio"
        )
      ) # 78-84s for rf

      print("*** DALEX.permutation.fi")

      # bar_width = 5.9 items / 12.4 composites
      ( bar.width <- 20 / log(ncol(features)) )

      DALEX.permutation.fi.plot <- DALEX.permutation.fi %>%
        plot(
          bar_width = bar.width
          , show_boxplots = FALSE
          , title = "Permutation Feature importance"
          , subtitle = ""
        ) +
        scale_y_continuous(expand = expansion()) # reduce space to axis

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

#####################################################
# calculate permutation feature importance
#####################################################
get_pimp_range_List_models_list <- function(
  list_models_list, model_string = "", value_function = max) {

  list_models_list %>%
    {
      if (model_string != "") {

        get_list_elements_by_string(., model_string)

      } else {
        .
      }
    } %>%
    {
      # TRICKY: temporary assignment works only for . not names(.) bec it
      # is piped through, so call models.selected %>% names later
      {
        . -> models.selected
      } %>%
        imap_dfr(function(models_list, models_list_label) {
          models_list %>%
            imap(function(model, model_label) {
              model$DALEX.permutation.fi %>%
                filter(variable != "_baseline_") %>%
                group_by(variable) %>%
                summarise(mean_dropout_loss = mean(dropout_loss)) %>%
                .$mean_dropout_loss %>%
                value_function
            })
        }) %>%
        as.data.frame %>%
      set_rownames(models.selected %>% names)
    }
}


################################################################################
# MAIN
################################################################################
# Step1) read models.lists from all datasets
################################################################################
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
# models.list <- datasets.models.list %>% pluck(1) # composites
# models.list <- datasets.models.list %>% pluck(5) # items

################################################################################
# Step2: get permutation feature importance for each models.list
################################################################################
NEW <- TRUE
# NEW <- FALSE

if (NEW) {
  getOption("parallelly.fork.enable")
  options(parallelly.fork.enable = TRUE)
  # plan(multisession)
  plan(multicore)
  system.time(
    datasets.permutation.fi.lists <- datasets.models.list %>%
      map(~ get_pimp_for_models_list(.x, no_permutations = 50))
  ) # 60s (B = 5) vs 499s (B = 50)

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

datasets.permutation.fi.lists %>% names

datasets.permutation.fi.lists %>%
  get_pimp_range_List_models_list("composites")

datasets.permutation.fi.lists %>%
  get_pimp_range_List_models_list("items")

################################################################################
# Step3: save feature importance plots
################################################################################
save_pimp_plots <- function(
  datasets_pimp_lists,
  model_string = "",
  scaling_factor = 1,
  width = "automatic",
  height = "automatic",
  axis_tick_labels = NULL) {

  pimp.max <- datasets_pimp_lists %>%
    get_pimp_range_List_models_list(model_string) %>%
    max %>%
    print

  pimp.min <- datasets_pimp_lists %>%
    get_pimp_range_List_models_list(model_string, min) %>%
    min %>%
    print

  no.features <- function(model) {
    model %>%
      pluck("DALEX.explainer") %>%
      pluck("data") %>%
      ncol %>% print
  }

  datasets_pimp_lists %>%
    get_list_elements_by_string(model_string) %>%
    imap(function(model, model_label) {
      model %>%
      imap(
        ~ .x %>%
          {
            .$DALEX.permutation.fi.plot +
              # same scale on flipped x-asis for same datasets_pimp_lists
              scale_y_continuous(limits = c(pimp.min, pimp.max)) +
              # next layer must be added within code block of plot object
              {
                if (!is.null(axis_tick_labels)) {
                  scale_x_discrete(labels = axis_tick_labels)
                }
              }
          } %>%
          ggsave(
            filename = paste0(c("figures/pimp", model_label, .y, "png"), collapse = "."),
            plot = .,
            scale = scaling_factor,
            width = ifelse(is.numeric(width), width, log(no.features(.x)) * 3),
            height = ifelse(is.numeric(height), height, log(no.features(.x)) * 2.2),
            dpi = 150
          )
      )
    })
}


# save feature importance plots
if (NEW) {

  datasets.permutation.fi.lists %>%
    save_pimp_plots("composites",
                    scaling_factor = 1.3,
                    width = 4,
                    height = 3,
                    axis_tick_labels = data.labels.long)

  system.time(
    datasets.permutation.fi.lists %>%
      save_pimp_plots("items",
                      scaling_factor = 0.9,
                      width = 9,
                      height = 8,
                      axis_tick_labels = data.labels.long)
  )
}
# fi.list <- datasets.permutation.fi.lists$PERF10.big5composites.all
fi.list <- datasets.permutation.fi.lists$PERF10.big5items.all

fi.list$LR$DALEX.permutation.fi
fi.list$LR$DALEX.permutation.fi %>% as_tibble
fi.list$LR$DALEX.permutation.fi %>% attributes
fi.list$LR$DALEX.permutation.fi %>% names

datasets.permutation.fi.lists %>%
  get_pimp_range_List_models_list("items", min)





