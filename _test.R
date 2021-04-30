
libraries <- c("dplyr", "magrittr", "tidyverse"
               , "sjlabelled" # read SPSS
               , "caret", "doParallel"
               , "DataExplorer", "RColorBrewer"
               , "machinelearningtools"
               , "knitr", "pander"
)
# devtools::install_github("agilebean/machinelearningtools")
# unloadNamespace("machinelearningtools")
sapply(libraries, require, character.only = TRUE)

source("_labels.R")

data.label <- "data/models.list.PERF10.big5composites.all.100repeats.noimpute.rds"
# data.label <- "data/models.list.PERF10.big5items.all.100repeats.noimpute.rds"
# don't confuse: models.list.PERF10.big5composites.80-20.100repeats.noimpute

model.permutations.string

models.list <- data.label %>% readRDS() %>% print
models.list$knn$trainingData %>% dim()


################################################################################
######################


######################
################################################################################


data.label <- "PERF10.big5composites"
# data.label <- "PERF10.big5items"

ci.results %>%
  # xtable::xtable(digits = 3) %>% print(type = "html")
  knitr::kable(digits = 3, format = "html") %>%
  cat(file = paste0("tables/ci ", data.label, ".html"))



myresamples <- mymodels %>% resamples %>% print


# all models

mymodels


model$finalModel %>% summary()

model$finalModel %>%
  predict(model$trainingData, se = TRUE)

model$finalModel %>%
  predict(model$trainingData, interval = "confidence")

model$finalModel %>%
  predict(models.list$testing.set, interval = "confidence")

model$finalModel %>%
  predict(models.list$testing.set)

# https://community.rstudio.com/t/prediction-intervals-with-tidymodels-best-practices/82594/4
library(tidyverse)

envir <- parsnip::get_model_env()

ls(envir) %>%
  tibble(name = .) %>%
  filter(str_detect(name, "_predict")) %>%
  mutate(prediction_modules  = map(name, parsnip::get_from_env)) %>%
  unnest(prediction_modules) %>%
  filter(str_detect(type, "pred_int"))



