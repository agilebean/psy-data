################################################################################
# Class:      Psychology Collaboration
# Topic:      Tenure and Job Performance
#
# Sources:    SPSS File: "Personality-Performance-Turnover-Chaehan So.sav"
#
################################################################################
# clear the workspace
rm(list=ls())

# mode <- "report.single"
mode <- "report.all"

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
libraries <- c("dplyr", "tidyverse", "magrittr", "knitr", "machinelearningtools")
sapply(libraries, require, character.only = TRUE)

PREFIX <- "output/psy-data-analysis"

system.time(
  if (mode == "report.single") {

    # select target and features
    target.label <- c("PERF10")
    features.set.label <- c("big5composites")
    job.label <- c("sales")

    output.filename <- output_filename(
      PREFIX,
      c(target.label, features.set.label, job.label),
      cv_repeats = CV.REPEATS, impute_method = IMPUTE.METHOD,
      suffix = "pdf"
    ) %>%
      # tricky: avoid render error for special character (R\&D) in output_file
      gsub("&", "", .)

    system.time(
      rmarkdown::render(input = "psy-data-analysis.Rmd",
                        params = list(target.label = target.label,
                                      features.set = features.set.label,
                                      job.label = job.label),
                        output_file = output.filename)
    )

  } else if (mode == "report.all") {

    target.label.list <- c("PERF10")
    features.set.labels.list <- c("big5items", "big5composites")
    # features.set.labels.list <- c("big5composites")
    job.labels.list <- c("sales", "R&D", "support", "all")

    model.permutations.labels <- crossing(
      target_label = target.label.list,
      features_set_label = features.set.labels.list,
      job_label = job.labels.list
    ) %T>% print

    render_report <- function(target_label, features_set_label, job_label) {

      output.filename <- output_filename(
        PREFIX,
        c(target_label, features_set_label, job_label),
        cv_repeats = CV.REPEATS, impute_method = IMPUTE.METHOD,
        suffix = "pdf"
      ) %>%
        # tricky: avoid render error for special character (R\&D) in output_file
        gsub("&", "", .)

      rmarkdown::render(input = "psy-data-analysis.Rmd",
                        params = list(target.label = target_label,
                                      features.set = features_set_label,
                                      job.label = job_label,
                                      cv.repeats = CV.REPEATS,
                                      impute.method = IMPUTE.METHOD),
                        output_file = output.filename)
    }

    system.time(
      model.permutations.labels %>% pmap_chr(render_report)
    )
  } # 142s/8 = 18s
)
