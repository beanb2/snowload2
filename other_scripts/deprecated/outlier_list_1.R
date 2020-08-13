# This script consolidates and saves all flagged outliers as a package dataset.
# The motivation for this approach is that the state and county maximums would
# often flag values that they shouldn't. Miranda Rogers and Scout Jarman have
# manually verified all of the flagged values in this dataset. Only the
# obvious outliers are removed prior to analysis.

# Read the outlier list created by Scout and Miranda
tfiles <- list.files(path = "data-raw/outlier_notes",
                     pattern = "Outliers.csv", full.names = TRUE)

temp <- vector("list", length(tfiles))
for(i in 1:length(tfiles)){
  temp[[i]] <- read.csv(tfiles[i])
}

outlier_final <- data.table::rbindlist(temp, fill = TRUE)

# Read in 1952 stations and remove from list.
t1952 <- read.csv("data-raw/1952report.csv")

# Resolve differing column names
outlier_final$OUTLIER[is.na(outlier_final$OUTLIER)] <-
  outlier_final$Outlier[is.na(outlier_final$OUTLIER)]

outlier_final$Outlier <- NULL

# Remove values that have the spooky 1952 problems
# (there is an easier way to remove them)
outlier_final2 <- outlier_final %>% dplyr::filter(!(is.element(ID, t1952$ID) &
                                ELEMENT == "WESD" &
                                lubridate::year(lubridate::as_date(DATE)) == 1952))

outlier_state_county <- outlier_final2
usethis::use_data(outlier_state_county, overwrite = TRUE)


# Data preparation for NOAA.
# library(tidyverse)
# outlier_noaa <- outlier_state_county %>%
#   dplyr::filter(OUTLIER > 0) %>%
#   dplyr::select(-FLAGGED, -OUTLIER)
#
# write.csv(outlier_noaa,
#           "data-raw/state_county_outlier_07062020.csv",
#           row.names = FALSE)
#
# # Determine number of stations with "spooky 1952" anomaly.
# scout <- read.csv("data-raw/spooky52counts.csv")
#
# scout2 <- unique(scout$ID)
#
# write.csv(scout2, file = "data-raw/spooky1952_stations.csv",
#           row.names = FALSE)
