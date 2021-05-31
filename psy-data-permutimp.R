################################################################################
# Script:     psy-data-varimp
# Class:      Psychology Collaboration
# Topic:      Tenure and Job Performance
#
################################################################################
# load libraries
detach("package:machinelearningtools", character.only = TRUE)
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
furrr_options(seed = TRUE)

source("_labels.R")
source("_common.R")

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
  future::supportsMulticore()

  getOption("parallelly.fork.enable")
  options(parallelly.fork.enable = TRUE)

  plan(multicore (workers = 8))

  system.time(
    datasets.permutation.fi.lists <- datasets.models.list %>%
      future_map(
        ~ get_pimp_for_models_list(.x, no_permutations = 100),
        .options = furrr_options(
          seed = TRUE
          , packages = c("dplyr", "DALEX")
        )
      ) %T>% {
        beepr::beep()
      }
  )
# B = 5: 119s
# B = 50: 315.2s multicore8  vs. 499-621s, map
# B = 100: 667s multicore8 vs. 1146s map

  system.time(
    datasets.permutation.fi.lists %>%
      saveRDS(permutation.fi.lists.label)
  ) # 71s (B50) 84s (B100)

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





