library(tidyverse)

# Miranda Rogers and Scout Jarman looked at scatterplots of snow load vs
# elevation for each state and identified points that seemed anomalous.
# These stations were flagged and their periods of record searched for
# outliers. The observations in this script were deemed "obvious" outliers
# that required further searching.

scout <- read.csv("data-raw/outlier_notes/scout_state_level_outlier.csv")
miranda <- read.csv("data-raw/outlier_notes/miranda_state_level_outlier.csv")

final <- dplyr::bind_rows(scout, miranda) %>%
  dplyr::filter(lubridate::year(lubridate::as_date(DATE)) != 1952 | ELEMENT != "WESD")

outlier_state_checks <- final

usethis::use_data(outlier_state_checks, overwrite = TRUE)
