# This script consolidates and saves all flagged outliers as a package dataset.
# The motivation for this approach is that the state and county maximums would
# often flag values that they shouldn't. Miranda Rogers and Scout Jarman have
# manually verified all of the flagged values in this dataset. Only the
# obvious outliers are removed prior to analysis.
library(tidyverse)

# 1: State and county outliers as flagged from state and county records for
# snowfall and snow depth.
#=============================================================================
# Read the outlier list created by Scout and Miranda
tfiles <- list.files(path = "data-raw/outlier_notes/state_county_outliers",
                     pattern = "Outliers.csv", full.names = TRUE)

tstates <- gsub("^.*/", "", tfiles)
tstates <- gsub("_.*$", "", tstates)

scout_state <- state.abb[c(1, 4:6, 12, 15:16, 23, 25:28,
                           31, 37, 43:44, 47, 50)]

sum(is.element(tstates, scout_state))

temp <- vector("list", length(tfiles))
for(i in 1:length(tfiles)){
  temp[[i]] <- read.csv(tfiles[i])

  if(is.element(tstates[i], scout_state)){
    temp[[i]]$name <- "Scout"
  }else{
    temp[[i]]$name <- "Miranda"
  }
}

outlier_final <- data.table::rbindlist(temp, fill = TRUE)

# Resolve differing column names
outlier_final$OUTLIER[is.na(outlier_final$OUTLIER)] <-
  outlier_final$Outlier[is.na(outlier_final$OUTLIER)]

outlier_final$Outlier <- NULL

outlier_final$TYPE <- "state_flag"

outlier_state_county <- outlier_final
#=============================================================================

# 2: Outliers identified from anomalous distribution fits.
#=============================================================================
# A series of stations with candidate outliers were determined by finding
# stations where the distribution fits were much different than the
# surrounding region. This led to the creation of outlier lists and
# "no confidence" stations. This script consolidates this information
# and creates an outlier dataset to check against.

# Function to consolidate outliers.
get_files <- function(path, pattern){
  tfiles <- list.files(path = path,
                       pattern = pattern, full.names = TRUE)

  temp <- vector("list", length(tfiles))
  for(i in 1:length(tfiles)){
    temp[[i]] <- read.csv(tfiles[i])

    temp[[i]]$name <- "unknown"

    if(regexpr("[Ss][Oo][Uu][Tt]", tfiles[i])){
      temp[[i]]$name <- "Scout"
    }

    if(regexpr("[Ss][Aa][Ll][Aa][Mm]", tfiles[i])){
      temp[[i]]$name <- "Salam"
    }

    if(regexpr("[Mm][Ii][Rr][Aa][Nn][Dd][Aa]", tfiles[i])){
      temp[[i]]$name <- "Miranda"
    }


    if(is.na(strptime(temp[[i]]$DATE[1], "%Y-%m-%d"))){
      temp[[i]]$DATE <- strptime(temp[[i]]$DATE, "%m/%d/%Y")
      temp[[i]]$DATE <- as.character(format(temp[[i]]$DATE, "%Y-%m-%d"))
    }

  }

  final <- data.table::rbindlist(temp, fill = TRUE)

  return(final)
}

# 1 - Remove Outliers at locations with particularly heavy tails.
heavy_outlier <- get_files(path = "data-raw/outlier_notes/distribution_outliers/",
                           pattern = "heavy_?1?_?outlier.csv")
heavy_outlier$TYPE <- "heavy_tail"

# 2 - Remove outliers at locations with suspiciously left-skewed distributions.
left_skew_outlier <- get_files(path = "data-raw/outlier_notes/distribution_outliers/",
                               pattern = "left_skew_outlier.csv")
left_skew_outlier$TYPE <- "left_skew"

# 3 - Remove outliers at locations with distribution shapes significantly different
# than the surrounding region.
region_outlier <- get_files(path = "data-raw/outlier_notes/distribution_outliers/",
                            pattern = "regional_outlier.csv")
region_outlier$TYPE <- "region_shape"


# 4 - Remove outliers where the 50 year event is more than double or less than
# half of the maximum max.
yr50_outlier <- get_files(path = "data-raw/outlier_notes/distribution_outliers/",
                          pattern = "50yr_outlier.csv")
yr50_outlier$TYPE <- "yr50"


# 5 - Remove outliers at stations that receive hardly any snow.
small_outlier <- get_files(path = "data-raw/outlier_notes/distribution_outliers/",
                           pattern = "small_check_outlier.csv")
small_outlier$TYPE <- "small_check"

# None of the small valued outliers are greater than zero. As such, we will not
# remove them from the record.

# Bind rows and save R object.
outlier_distribution <- dplyr::bind_rows(heavy_outlier, left_skew_outlier,
                                         region_outlier, yr50_outlier)
#=============================================================================

# 3: Remove values identified by our team as having anomalous 50 year
# or RT loads based on scatterplot observations at the state level.
#=============================================================================
# Miranda Rogers and Scout Jarman looked at scatterplots of snow load vs
# elevation for each state and identified points that seemed anomalous.
# These stations were flagged and their periods of record searched for
# outliers. The observations in this script were deemed "obvious" outliers
# that required further searching.
scout <- read.csv("data-raw/outlier_notes/scout_state_level_outlier.csv")
scout$name <- "Scout"
miranda <- read.csv("data-raw/outlier_notes/miranda_state_level_outlier.csv")
miranda$name <- "Miranda"

final <- dplyr::bind_rows(scout, miranda)

outlier_state_checks <- final
outlier_state_checks$TYPE <- "state_scatter"
#=============================================================================

# 4: "Last minute" outliers based on observations made by the committee based
# on initial results.
#=============================================================================
# Read in Miranda's additional "heavy tail" outliers.
miranda_last <- read.csv("data-raw/outlier_notes/outlier_check_07212020.csv")
miranda_last$name <- "Miranda"
miranda_last$TYPE <- "heavy_tail_2"

salam_last <- read.csv("data-raw/outlier_notes/salam_ORWA_outlier.csv")
salam_last$DATE <- strptime(salam_last$DATE, "%m/%d/%Y")
salam_last$DATE <- as.character(format(salam_last$DATE, "%Y-%m-%d"))
salam_last$name <- "Salam"
salam_last$TYPE <- "OR_WA_check"


outlier_last_minute <- dplyr::bind_rows(miranda_last, salam_last)
#=============================================================================

# 5: Outliers found by site-specific investigations by Scout in
# - Alaska
# - national scatterplot checks (I think)
# - Amboy, IL
# - Colorado eastern plains
# - The border of North and South Dakota (need to add)
#=============================================================================
# Alaskan Outliers.
scout_ak <- read.csv("data-raw/outlier_notes/outliers_ak.csv")

# Can't remember exactly which investigation this was from, but I think it was
# outliers Scout found after checking out scatterplots of the data.
# Remove spurious row name column.
scout_1 <- read.csv("data-raw/outlier_notes/scouts_outliers.csv")[, -1]

# Scout's investigation of Amboy, IL at the request of karl.r.pennings@imegcorp.com
scout_2 <- read.csv("data-raw/outlier_notes/AMBOY_outliers.csv")

# Scouts investigation of the Eastern Plains of Colorado, with a focus on the
# Denver area.
scout_3 <- read.csv("data-raw/outlier_notes/stations_1_outliers.csv")

scout_final_checks <- dplyr::bind_rows(scout_ak, scout_1,
                                       scout_2, scout_3)

scout_final_checks$name <- "Scout"
scout_final_checks$TYPE <- "Scout_Final_Investigations"
#=============================================================================

# Save the results
#=============================================================================
# 7-29-2021: This line inadvertently removes values where there was a discrepancy
# in the outlier flags. outlier_list_master in the reproducible version fixes
# this problem
outlier_combined <- data.table::rbindlist(list(outlier_state_county,
                                               outlier_distribution,
                                               outlier_state_checks,
                                               outlier_last_minute,
                                               scout_final_checks), fill = TRUE) %>%
  dplyr::mutate(YEAR = lubridate::year(lubridate::as_date(DATE))) %>%
  dplyr::filter(YEAR != 1952 | ELEMENT != "WESD") %>%
  dplyr::distinct(ID, DATE, ELEMENT, VALUE, .keep_all = TRUE)




save(outlier_combined, file = "data-raw/outliers_with_names_scout.RData")
#=============================================================================

# Recent changes (07-31-2020) only retain stations with at least 5 years of
# record passing coverage filters. This eliminates the need for the no
# confidence station lists.
# no_confidence_stations <- get_files(path = "data-raw/outlier_notes/distribution_outliers/",
#                                     pattern = "confidence.csv")[, 1]
# usethis::use_data(no_confidence_stations, overwrite = TRUE)
#=============================================================================

