# Adjusted script that eliminates some redundant outliers. Saved as a separate
# script because a number of small changes made it difficult to update the
# original file.

# Script to join Scouts new checks to the original outlier file.
library(tidyverse)
library(snowload2)

tfile <- list.files("data-raw/outlier_checks/",
                    pattern = "_[[:digit:]].csv", full.names = TRUE)

files <- vector("list", length(tfile))
for(i in 1:3){
  files[[i]] <- read.csv(tfile[[i]])
}

scout_final <- data.table::rbindlist(files)

# Remove all of Scout's outliers that were already flagged by the GHCND.
tid <- unique(scout_final$ID)

tvalues <- get_station_data(tid, source = "C:/Users/Brennan/Desktop/rt_snow_final/data/ghcnd/ghcnd_all_08252020/")
tvalues2 <- tvalues %>% dplyr::filter(QFLAG != " ") %>%
  dplyr::select(ID, DATE, ELEMENT, VALUE) %>%
  dplyr::mutate(DATE = as.character(DATE))

scout_final2 <- dplyr::anti_join(scout_final, tvalues2,
                                 by = c("ID", "DATE", "ELEMENT")) %>%
  dplyr::mutate(YEAR = lubridate::year(lubridate::as_date(DATE))) %>%
  dplyr::filter(YEAR != 1952 | ELEMENT != "WESD") %>%
  dplyr::arrange(ID, DATE, dplyr::desc(abs(OUTLIER))) %>%
  dplyr::distinct(ID, DATE, ELEMENT, VALUE, .keep_all = TRUE)

# Read in the named version of the outliers.
# load("data-raw/outliers_with_names_scout.RData")
load("data-raw/outlier_checks/outliers_with_names_scout.RData")
outlier_combined2 <- outlier_combined

# Compare to previous values.
outlier_combined <- snowload2::outlier_combined

# Join the values.
new_outlier <- dplyr::full_join(outlier_combined2, scout_final2,
                                by = c("ID", "DATE", "ELEMENT", "VALUE", "YEAR")) %>%
  tidyr::replace_na(list(OUTLIER.x = 0, FLAGGED.x = 0, FLAGGED.y = 0, OUTLIER.y = 0))

# Retain if one of the following conditions are met:
# - Scout checked it originally
# - Scout flagged it the second time around
# - The original value was zero (indicating artificial zero removed)
# - The original value was flagged as an aritificial low year (-1 outlier)
scout_outlier <- new_outlier %>%
  dplyr::filter(name == "Scout" | OUTLIER.y != 0 | VALUE == 0 | OUTLIER.x <= 0)

outlier_combined_alt <- scout_outlier %>%
  dplyr::filter(OUTLIER.x != 0 | OUTLIER.y != 0)

# The filters should only keep outliers Scout flagged or verified.
# Make sure that the outlier lists reflect this.
outlier_combined_alt$OUTLIER <- outlier_combined_alt$OUTLIER.x
outlier_combined_alt$OUTLIER[outlier_combined_alt$OUTLIER == 0] <-
  outlier_combined_alt$OUTLIER.y[outlier_combined_alt$OUTLIER == 0]

outlier_combined_alt <- outlier_combined_alt %>%
  dplyr::select(ID, DATE, ELEMENT, VALUE, OUTLIER, YEAR) %>%
  dplyr::arrange(ID, DATE, dplyr::desc(abs(OUTLIER))) %>%
  dplyr::distinct(ID, DATE, ELEMENT, VALUE, .keep_all = TRUE)


usethis::use_data(outlier_combined_alt, overwrite = TRUE)

write.csv(outlier_combined_alt,
          file = "data-raw/scout_outlier_check_combined.csv",
          row.names = FALSE)


# See what was left out
#=============================================================================
# Determine which values are not in the new list that were in the original.
left_out <- dplyr::anti_join(outlier_combined, outlier_combined_alt,
                             by = c("ID", "DATE", "ELEMENT", "VALUE")) %>%
  dplyr::filter(OUTLIER != 0)

left_out2 <- dplyr::anti_join(outlier_combined_alt, outlier_combined,
                              by = c("ID", "DATE", "ELEMENT", "VALUE")) %>%
  dplyr::filter(OUTLIER != 0)

tid <- unique(left_out$ID)

data(ghcnd_stations)

st_map <- ghcnd_stations[is.element(ghcnd_stations$ID, tid), ]

plot(st_map$LONGITUDE, st_map$LATITUDE)

# There is very little overlap between Scout's new lists and the original lists...
# which is a little disconcerting.
comb_outlier <- dplyr::inner_join(outlier_combined2, scout_final2,
                                  by = c("ID", "DATE", "ELEMENT", "VALUE"))
#=============================================================================


