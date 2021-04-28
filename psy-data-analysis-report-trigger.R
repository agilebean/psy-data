################################################################################
# Paper:      Psy-paper
# Target:     Job Performance
#
# Sources:    SPSS File: "Personality-Performance-Turnover-Chaehan So.sav"
#
################################################################################
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

PREFIX.results <- "results/psy-data-analysis"

source("_labels.R")

render_single_report <- function(target_label, features_set_label, job_label) {

  output.filename <- output_filename(
    PREFIX.results,
    target_label, features_set_label, job_label,
    paste0(CV.REPEATS, "repeats"), impute_method = IMPUTE.METHOD,
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
  if (mode == "report.single") {

    # select target and features
    target.label <- c("PERF10")
    features.set.label <- c("big5composites")
    # job.label <- c("sales")
    job.label <- c("all")

    render_single_report(
      target_label = target.label,
      features_set_label = features.set.label,
      job_label = job.label
    )

  } else if (mode == "report.all") {

    system.time(
      model.permutations.labels %>% pmap_chr(render_single_report)
    )
  } # 123s/8 = 18s
)
