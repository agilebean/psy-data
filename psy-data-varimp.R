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
  , "sjlabelled" # read SPSS
  , "caret", "doParallel"
  , "RColorBrewer"
  , "machinelearningtools"
  , "knitr"
  , "RPushbullet", "beepr"
  , "tidyverse"
)
sapply(libraries, require, character.only = TRUE)

# target.label.list <- c("LIFE_S_R", "PERF09", "PERF10",  "PERF11")
target.label.list <- c("PERF10")
# target.label.list <- c("PERF09")
# features.set.labels.list <- c("big5items", "big5composites")
# features.set.labels.list <- c("big5composites")
features.set.labels.list <- c("big5items")
job.labels.list <- c("sales", "R&D", "support", "all")

models.varimp <- models.list %>%
  names %>%
  str_detect("lm|glmnet|gbm|rf") %>%
  # select specific list elements by name
  purrr::keep(models.list, .)

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

model.permutations.labels
# get model in model.permutations.list by model index
model.index = 1
model.index.labels <- model.permutations.labels %>% .[model.index,] %T>% print
target_label <- model.index.labels$target_label
features_set_label <- model.index.labels$features_set_label
job_label <- model.index.labels$job_label
CV.REPEATS <- 100

# prefix
models.list.name <- output_filename(
  PREFIX,
  c(target_label, features_set_label, job_label),
  paste0(CV.REPEATS, "repeats"), impute_method = IMPUTE.METHOD
) %>% print

# get model in model.permutations.labels by model index
models.list <- readRDS(models.list.name)
# models.list <- readRDS("data/models.list.PERF10.big5items.100repeats.noimpute.rds")

models.varimp <- models.list %>%
  names %>%
  str_detect("lm|glmnet|gbm|rf") %>%
  # select specific list elements by name
  purrr::keep(models.list, .)

visualize_importance <- function (importance_object, relative = FALSE) {

  require(gbm)
  # importance_object <- models.varimp$rf %>% varImp
  # importance_object %>% class

  unit.label <- ifelse(relative, "%RI", "importance") %T>% print
  unit.variable <- rlang::sym(unit.label)


  if (class(importance_object) == "varImp.train") {
    importance_object %<>% .$importance
  }
  if (!hasName(importance_object, "rowname")) {
    importance_object %<>% rownames_to_column()
  }

  importance.table <- importance_object %>%
    rename(variable = rowname, importance = Overall) %>%
    arrange(desc(importance)) %>%
    {
      if (relative) {
        mutate(., `%RI` = importance/sum(importance)*100) %>%
          select(variable, `%RI`)
      } else {
        .
      }
    } %T>% print

  importance.plot <- importance.table %>%
    set_names(c("variable", unit.label)) %>%
    ggplot(data = .,
           aes(x = reorder(variable, !!unit.variable), y = !!unit.variable)) +
    theme_minimal() +
    geom_bar(stat = "identity", fill = "#114151") +
    coord_flip() +
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 12)) +
    # scale_y_continuous(expand = c(0, 0), limits = c(0, 102)) +
    labs(
      x = "item",
      y = unit.label
    )

  return(
    list(
      importance.table = importance.table,
      importance.plot = importance.plot
    ))
}


tables.varimp <- models.varimp %>% map(~varImp(.)) %T>% print

varimp.list <- models.varimp %>%
  map(function(model) {
    model %>% varImp %>% visualize_importance()
  })

varimp.list

varimp.list$lm
varimp.list$glmnet
varimp.list$gbm
varimp.list$rf

