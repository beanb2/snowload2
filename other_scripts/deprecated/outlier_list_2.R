library(tidyverse)

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
  }

  final <- data.table::rbindlist(temp, fill = TRUE)

  return(final)
}

# Step 1: Save the no confidence stations.
#=============================================================================
no_confidence_stations <- get_files(path = "data-raw/outlier_notes/distribution_outliers/",
                                     pattern = "confidence.csv")[, 1]

usethis::use_data(no_confidence_stations, overwrite = TRUE)
#=============================================================================

# Step 2: Document the recorded outliers from Scout, Miranda, and Salam.
#=============================================================================
# 1 - Remove Outliers at locations with particularly heavy tails.
heavy_outlier <- get_files(path = "data-raw/outlier_notes/distribution_outliers/",
                            pattern = "heavy_?1?_?outlier.csv")
heavy_outlier$TYPE <- "heavy-tail"

# 2 - Remove outliers at locations with suspiciously left-skewed distributions.
left_skew_outlier <- get_files(path = "data-raw/outlier_notes/distribution_outliers/",
                                pattern = "left_skew_outlier.csv")
left_skew_outlier$TYPE <- "left_skew"

# 3 - Remove outliers at locations with distribution shapes significantly different
# than the surrounding region.
region_outlier <- get_files(path = "data-raw/outlier_notes/distribution_outliers/",
                               pattern = "regional_outlier.csv")
region_outlier$TYPE <- "region-shape"


# 4 - Remove outliers where the 50 year event is more than double or less than
# half of the maximum max.
yr50_outlier <- get_files(path = "data-raw/outlier_notes/distribution_outliers/",
                            pattern = "50yr_outlier.csv")
yr50_outlier$TYPE <- "yr50"


# 5 - Remove outliers at stations that receive hardly any snow.
small_outlier <- get_files(path = "data-raw/outlier_notes/distribution_outliers/",
                          pattern = "small_check_outlier.csv")
small_outlier$TYPE <- "small-check"

# None of the small valued outliers are greater than zero. As such, we will not
# remove them from the record.

# Bind rows and save R object.
# No need to save "Spooky 1952 outliers since we are already removing them.
outlier_distribution <- dplyr::bind_rows(heavy_outlier, left_skew_outlier,
                                         region_outlier, yr50_outlier) %>%
  dplyr::mutate(YEAR = lubridate::year(lubridate::as_date(DATE))) %>%
  dplyr::filter(YEAR != 1952 | ELEMENT != "WESD") %>%
  dplyr::select(-YEAR)

usethis::use_data(outlier_distribution, overwrite = TRUE)
#=============================================================================



