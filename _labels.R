# target.label.list <- c("LIFE_S_R", "PERF09", "PERF10",  "PERF11")
target.label.list <- c("PERF10")
# target.label.list <- c("PERF09")
# features.set.labels.list <- c("big5items", "big5composites")
# features.set.labels.list <- c("big5composites")
features.set.labels.list <- c("big5items")
job.labels.list <- c("sales", "R&D", "support", "all")


model.permutations.labels <- crossing(
  target_label = target.label.list,
  features_set_label = features.set.labels.list,
  job_label = job.labels.list
)
# model.permutations.labels <- model.permutations.labels[-c(1:3),]

model.permutation.string <- model.permutations.labels %>%
  pmap_chr(function(target_label, features_set_label, job_label) {
    paste(target_label, features_set_label, job_label, sep = "-")
  })

# nominal <- FALSE # with ordinal as ORDERED factors
nominal <- TRUE # with ordinal as NOMINAL factor

seed <- 171

# cross-validation repetitions
# CV.REPEATS <- 2
# CV.REPEATS <- 10
CV.REPEATS <- 100

# try first x rows of training set
# TRY.FIRST <- NULL
TRY.FIRST <- 50
# TRY.FIRST <- 100

# split ratio
SPLIT.RATIO <- 1.0

# imputation method
IMPUTE.METHOD <- "noimpute"
# IMPUTE.METHOD <- "knnImpute"
# IMPUTE.METHOD <- "bagImpute"

# prefix
PREFIX <- "data/models.list"
# PREFIX <- "data/testruns/models.list"

# model labels for publication tables
models.labels.published <- c("LR", "kNN", "GBM", "RF", "SVM")

# color scheme
color.scheme <- c(
  "kNN" = "#F8766D",
  "SVM" = "#D55E00",
  "RF" = "#CC79A7",
  "GBM" = "#E69F00",
  "LR" = "#0072B2"
)
