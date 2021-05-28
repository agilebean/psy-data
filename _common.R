
read_models_list <- function(models_labels) {

  # get models label
  models.list.label <- output_filename(
    PREFIX, models_labels,
    paste0(CV.REPEATS, "repeats"), impute_method = IMPUTE.METHOD
  )

  # get model in model.permutations.labels by model index
  print(paste0("Reading file >> ", models.list.label))
  models.list.all <- readRDS(models.list.label)

  models.list.select <- models.list.all %>%
    list_modify(
      glmnet = NULL,
      kknn = NULL,
      xgbTree = NULL,
      xgbLinear = NULL,
      svmLinear = NULL,
      ranger = NULL) %>%
    list_modify(target.label = NULL, testing.set = NULL) %>%
    set_names(models.labels.published)

  return(models.list.select)

}
