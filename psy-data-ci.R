################################################################################
# Paper:      Psy-paper
# Target:     Job Performance
# Goal:       calculate confidence intervals
#
################################################################################

mode <- "ci.single"
# mode <- "ci.all"

# CV.REPEATS <- 10
CV.REPEATS <- 100
# IMPUTE.METHOD <- NULL
# IMPUTE.METHOD <- "medianImpute"
IMPUTE.METHOD <- "noimpute"
# IMPUTE.METHOD <- "knnImpute"
# IMPUTE.METHOD <- "bagImpute"

# load libraries
# devtools::install_github("agilebean/machinelearningtools", force = TRUE)
# detach("package:machinelearningtools", character.only = TRUE)
libraries <- c("dplyr", "tidyverse", "knitr", "machinelearningtools", "infer")
sapply(libraries, require, character.only = TRUE)

source("_labels.R")
source("_common.R")


PREFIX <- "results/psy-data-analysis"


calculate_ci_bootstrapped <- function(
  model, metric = "pred", repetitions = 100) {

  if (metric == "pred") {
    scores <- model$pred %>% specify(response = pred)

  } else if (metric == "RMSE") {

    system.time(
      scores <- model$resample %>% specify(response = RMSE)
    )

  } else if (metric == "R") {

    scores <- model$resample %>%
      mutate(R = sqrt(Rsquared)) %>%
      specify(response = R)
  }

  boot_distr <- scores %>%
    generate(reps = repetitions, type = "bootstrap") %>%
    calculate(stat = "mean")

  sample_mean <- scores %>%
    calculate(stat = "mean") %>%
    dplyr::pull()

  bb <- boot_distr %>%
    get_confidence_interval(
      point_estimate = sample_mean,
      level = 0.95,
      type = "se"
    )
}


calculate_ci_per_models_list <- function(
  models_list, metric, repetitions, digits = 3, save_label = "", ...) {

  ci.results <- map_dfr(
    models_list,
    ~ calculate_ci_bootstrapped(.x, metric, repetitions),
    .id = "model") %>%
    {
      if (metric == "R" | metric == "Rsquared") {
        arrange(., desc(upper_ci))
      } else {
        arrange(., upper_ci)
      }
    }


  if (save_label != "") {
    ci.results %>%
      convert_kable(., digits = digits, ...) %>%
      {
        if (save_label != "") {
          cat(., file = paste0(save_label, ".html"))
        }
        else {
          .
        }
      }
  }

  return(ci.results)
}


system.time(
  if (mode == "ci.single") {

    # step1: select labels
    target.label <- c("PERF10")
    features.set.label <- c("big5composites")
    # features.set.label <- c("big5items")
    # job.label <- c("sales")
    job.label <- c("all")

    # step2: read model list
    model.list <- read_models_list(
      c(target.label, features.set.label, job.label))

    # step3: calculate CIs
    metric <- "R"
    # metric <- "RMSE"
    ci.label <- paste0(c(
        "tables/ci.table",
        target.label, features.set.label, job.label, metric
        ), collapse = ".") %>% print

    best.model.CI <- calculate_ci_per_models_list(
      model.list, metric = "R",
      save_label = ci.label,
      repetitions = 10e4
      ) %T>% print


  } else if (mode == "ci.all") {

    # step1: define metric
    metric <- "R"
    # metric <- "RMSE"

    set.seed(171)
    system.time(
      ci.results <- model.permutations.labels %>%
        pmap(
          # step2: read model list
          ~ read_models_list(..1, ..2, ..3) %>%
            # step3: calculate CIs
            calculate_ci_per_models_list(
              .,
              metric = metric,
              repetitions = 10e4,
              save_label = paste0(
                list("tables/ci.table", ..1, ..2, ..3, metric),
                collapse = ".")
            )
        )
    ) # R: 11s/10rep, 22.3s/1000rep, 484s/10e4rep
    # RMSE: 498s/10e4rep
  }
)
