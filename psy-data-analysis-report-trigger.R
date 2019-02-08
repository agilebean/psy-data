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

# load libraries
# devtools::install_github("agilebean/machinelearningtools", force = TRUE)
libraries <- c("dplyr", "magrittr", "tidyverse", "purrr")
sapply(libraries, require, character.only = TRUE)


if (mode == "report.single") {
  
  # select target and features
  target.label <- "PERF.all"
  # target.label <- "PERF09"
  
  features.set <- "big5items"
  # features.set <- "big5composites"
  output.filename <- paste0(c("output/psy-data-analysis", 
                              target.label, features.set, "pdf"),
                            collapse = ".") %>% print
  
  system.time(
    rmarkdown::render(input = "psy-data-analysis.Rmd",
                      params = list(target.label = target.label,
                                    features.set = features.set),
                      output_file = output.filename)
  )
  
} else if (mode == "report.all") {
  
  target.label.list <- c("PERF09", "PERF.all", "TO.all")
  features.set.list <- c("big5items", "big5composites")
  
  model.permutations.list <- crossing(target.label = target.label.list, 
                         features.set = features.set.list)
  
  render_report <- function(target.label, features.set) {
    
    output.filename <- paste("output/psy-data-analysis", target.label, features.set, "pdf", sep = ".") %T>% print
    
    rmarkdown::render(input = "psy-data-analysis.Rmd",
                      params = list(target.label = target.label,
                                    features.set = features.set),
                      output_file = output.filename)
  }

  system.time(
    model.permutations.list %>% pmap_chr(render_report)
  )
}
