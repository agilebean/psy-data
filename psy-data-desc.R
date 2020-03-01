
libraries <- c("dplyr", "magrittr", "tidyverse", "purrr"
               , "sjlabelled" # read SPSS
               , "caret", "doParallel"
               , "stargazer", "DataExplorer", "skimr"
               , "machinelearningtools"
               , "knitr", "pander"
)
sapply(libraries, require, character.only = TRUE)

# nominal <- FALSE # with ordinal as ORDERED factors
nominal <- TRUE # wit

filename <- "data/Personality-Performance-Turnover-Chaehan So.sav"

file.raw <- sjlabelled::read_spss(filename,
                                  atomic.to.fac = TRUE,
                                  verbose = FALSE)

data.labels <- foreign::read.spss(filename) %>%
  attributes %>%
  .$variable.labels %T>% print

file.raw %<>%
  dplyr::select(-id, -TESTDATE) %>%
  tbl_df

file.raw %>%
  filter(job != 1 & job != 10) %>%
  count()

data.raw <- file.raw %>%
  # convert categorical variables to factors
  mutate_at((c("COMNAME", "team_id", "class", "job", "gender", "educa")),
            as.factor) %>%
  # convert numerical variables to numeric datatype
  mutate_at(vars(starts_with("TO")), as.numeric) %>%
  # fix import error for PERF07
  mutate_at("PERF07", as.numeric) %T>% print

dataset <- data.raw %>% select(job, gender, educa, prinum) %>% print

# job type
dataset %>%
  group_by(job) %>%
  tally()

# gender
dataset %>%
  group_by(gender) %>%
  tally() %>%
  mutate(perc = n/sum(n)*100)

# age
dataset %>%
  separate(prinum, c("year", "monthday"), sep = 2) %>%
  select(-monthday) %>%
  # calculate age from year 2010 (PERF10)
  mutate(age = 110 - as.numeric(year)) %>%
  psych::describe()

# education
dataset %>%
  group_by(educa) %>%
  tally() %>%
  mutate(perc = n/sum(n)*100)
