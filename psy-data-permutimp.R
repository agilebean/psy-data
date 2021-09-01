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
# 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
# set_github_pat(force_new = FALSE, validate = interactive(), verbose = TRUE)
libraries <- c(
  "magrittr"
  , "caret"
  , "RColorBrewer"
  , "machinelearningtools"
  , "gbm"
  , "tidyverse"
  , "DALEX"
  , "furrr"
  , "vip"
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
  ) # 6.5-7.7s
}

datasets.models.list %>% names
# models.list <- datasets.models.list %>% pluck(1) # composites
# models.list <- datasets.models.list %>% pluck(5) # items

################################################################################
# Step2: get permutation feature importance for each models.list
################################################################################
## vip
##
datasets.models.list %>% names
datasets.models.list$PERF10.big5items.all %>% names
datasets.models.list$PERF10.big5items.all$RF %>% class

model <- datasets.models.list$PERF10.big5items.all$RF
# model <- datasets.models.list$PERF10.big5items.all$GBM
# model <- datasets.models.list$PERF10.big5items.all$kNN # nope
# model <- datasets.models.list$PERF10.big5items.all$SVM # nope

model %>% names
model$resample %>% dim
model$times
model$trainingData


benchmark_optimal_cores_vip <- function(no_cores_list) {

  benchmark.vip <- no_cores_list %>%

    map(function(no_cores) {

      clus <- clusterOn(no_cores = no_cores)
      # clusterEvalQ(clus, library(vip))

      times <- system.time(
        model.vi <- model %>%
          vi(
            method = "permute",
            target = ".outcome",
            train = model$trainingData,
            type = "ratio",
            metric = "rmse",
            keep = TRUE,
            parallel = TRUE,
            nsim = 100,
            pred_wrapper = predict
          )
      ) %T>% { beepr::beep() }

      clusterOff(clus)
      stopImplicitCluster()

      return(list(
        times = times,
        # !!sym(paste0("times", no_cores)) = times,
        model.vi = model.vi)
      )
    }) %>%
    set_names(no.cores.list)

  return(benchmark.vip)
}

no.cores.list <- c(1, 2, 4, 6, 8)

if (NEW) {
  benchmark_optimal_cores_vip(no.cores.list)

  benchmark.vip %>%
    saveRDS(benchmark.pfi.vip.label)
} else {
  benchmark.vip  <- readRDS(benchmark.pfi.vip.label)
}

# best 2 cores = 121s, 8 cores = 180s
model.times <- benchmark.vip %>%
  map_df(~ pluck(.x, "times")) %>%
  mutate(no.cores = no.cores.list, .before = everything()) %>%
  suppressWarnings() %>%
  select(-ends_with("child")) %>%
  arrange(elapsed) %>% print

model.times %>%
  ggplot(aes(x = no.cores, y = elapsed)) +
  geom_col(fill = "royalblue", width = 0.6) +
  coord_flip() +
  theme_linedraw() +
  labs(
    title = "Benchmark permutation feature importance by vip::vi_permute()",
    subtitle = "100 simulations, cluster by doParallel::makeForkCluster"
  )

model.vi <- benchmark.vip$`2`$model.vi
# model.vi %>% add_sparklines(model) # takes long

model.vi %>%
  vip(
    num_features = model$trainingData %>% ncol - 1,
    # all_permutations = TRUE, # shows dots
    # jitter = TRUE, # for dots
    include_type = TRUE,
    # geom = "boxplot"
    # geom = "violin"
    , aesthetics = list(fill = "royalblue")
  ) +
  scale_x_discrete(labels = data.labels.long) +
  # scale_y_continuous(limits = c(1, 2), expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  coord_flip(ylim = c(1, 2), expand = 0) +
  theme_minimal()

##
################################################################################

# NEW <- TRUE
NEW <- FALSE

SEED <- 17
if (NEW) {


  getOption("parallelly.fork.enable")
  options(parallelly.fork.enable = TRUE)
  future::supportsMulticore()
  future::availableCores()

  plan(multicore (workers = 2))

  system.time(
    datasets.permutation.fi.lists <- datasets.models.list %>%
      future_map(
        ~ get_pimp_for_models_list(
          .x,
          no_permutations = 100,
          seed = SEED),
        .options = furrr_options(
          seed = SEED
          , packages = c("dplyr", "DALEX")
        )
      ) %T>% {
        beepr::beep()
      }
  )
  # B = 5: 44s vs. 86s
  # B = 50: 315.2s multicore8  vs. 499-621s, map
  # B = 100: 667s,789s, 869s multicore8 894s (15) vs. 1146s map
  #

  system.time(
    datasets.permutation.fi.lists %>%
      saveRDS(permutation.fi.lists.label) %T>% {
        beepr::beep()
      }
  ) # 71s (B50) 84s (B100)

} else {

  system.time(
    datasets.permutation.fi.lists <-
      readRDS(permutation.fi.lists.label)
  ) # 13s (B50) 27s (B100)

}

datasets.permutation.fi.lists %>% names

datasets.permutation.fi.lists %>%
  get_pimp_range_List_models_list("composites")

datasets.permutation.fi.lists %>%
  get_pimp_range_List_models_list("items")


#####
# PUBLICATION INFO: PFI same factor across models
#####
show_pimps <- function (list_models_list, model_string = "") {

  list_models_list %>% {
    if (model_string != "") {
      get_list_elements_by_string(., model_string)
    }
    else {
      .
    }
  } %>%
    {
      {
        models.selected <- .
      } %>%
        imap(function(models_list, models_list_label) {

          models_list %>%
            imap_dfc(function(model, model_label) {

              models_sym <- rlang::sym(model_label)

              model$DALEX.permutation.fi %>%
                as_tibble %>%
                select(variable, dropout_loss) %>%
                filter(variable != "_baseline_" & variable != "_full_model_") %>%
                group_by(variable) %>%
                summarise(!!models_sym := mean(dropout_loss)) %>%
                arrange(desc(!!models_sym)) %>%
                {
                  { current_model <- . } %>%
                    select(!!models_sym) %>%
                    as.data.frame %>%
                    set_rownames(current_model$variable)
                }
            })
        })
    } %>%
    map( ~ .x %>% mutate(PFI.across = rowMeans(.x)))
}

datasets.permutation.fi.lists %>% show_pimps("composites")

datasets.permutation.fi.lists %>% show_pimps("items")

################################################################################
# Step3: save feature importance plots
################################################################################
# save feature importance plots
if (NEW) {

  system.time(
    datasets.permutation.fi.lists %>%
      save_pimp_plots("composites",
                      scaling_factor = 1.3,
                      width = 4,
                      height = 3,
                      axis_tick_labels = data.labels.long)
  ) # 5.6s

  system.time(
    datasets.permutation.fi.lists %>%
      save_pimp_plots("items",
                      scaling_factor = 0.9,
                      width = 9,
                      height = 8,
                      axis_tick_labels = data.labels.long)
  ) # 8.9s
}
# fi.list <- datasets.permutation.fi.lists$PERF10.big5composites.all
fi.list <- datasets.permutation.fi.lists$PERF10.big5items.all

fi.list$LR$DALEX.permutation.fi
fi.list$LR$DALEX.permutation.fi %>% as_tibble
fi.list$LR$DALEX.permutation.fi %>% attributes
fi.list$LR$DALEX.permutation.fi %>% names

datasets.permutation.fi.lists %>%
  get_pimp_range_List_models_list("items", min)





