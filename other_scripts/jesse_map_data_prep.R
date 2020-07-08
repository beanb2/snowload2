library(tidyverse)

# Set Variables and Filter and Cluster Stations
# ==============================================================================

# source of raw data
#source <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/" # Download
source <- "data-raw/ghcnd_all/" # Local

# used for converting values to loads
mm_water_to_kpa <- function(x) {x * 0.0098064}

# filtering and clustering stations
states <- c(
  "AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "ID", "IL", "IN",
  "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
  "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
  "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)
canada <- c(
  "AB", "BC", "MB", "NB", "NS", "ON", "PE", "QC", "SK"
)
stations <- snowload2::ghcnd_stations %>%
  dplyr::filter(!is.na(ELEVATION),
                STATE %in% states | STATE %in% canada,
                LATITUDE <= 51) %>%
  group_by(ECO3) %>%
  mutate(CLUST = snowload2::cluster_stations(LONGITUDE, LATITUDE, ELEVATION,
                                             dist_adj = 1,
                                             elev_adj = 16,
                                             h = 4)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(CLUST = sprintf(paste0(ECO3, ".%04d"), CLUST))

# Ecoregions over which to partition the data
all_eco3 <- sort(unique(as.character(stations$ECO3)))

# final output
swe_max <- vector("list", length = length(all_eco3))

# Process for all partitions
for (eco3 in all_eco3) {

  print(paste("Processing for eco-region", eco3))

  # Gather, Convert, and Clean Station Data
  # ============================================================================
  raw <- snowload2::get_region_data(eco3, source)

  model_input <- raw %>%
    dplyr::left_join(stations) %>%
    dplyr::rename(ELEV = ELEVATION) %>%
    dplyr::mutate(SMONTH = (as.numeric(lubridate::month(DATE)) + 3) %% 12,
                  logSNWD = if_else(VALUE == 0, 0, log(VALUE))) %>%
    dplyr::filter(ELEMENT == "SNWD")

  clean <- raw %>%
    dplyr::left_join(stations %>% dplyr::select(ID, CLUST)) %>%
    snowload2::clean_station_data(c("mpzero", "qflag", "month", "high_max"),
                                  months = c(7, 8, 9), max_grouping = CLUST,
                                  iqr_cutoff = 3, max_yearly_outliers = 5)


  # Find Yearly Maximums
  # ============================================================================
  ym <- clean %>%
    snowload2::yearly_maximums(id = CLUST, date = DATE, value = VALUE,
                               value_type = ELEMENT, prioritize = "WESD",
                               prioritize_threshold = list(n = 10,
                                                           months = 3)) %>%
    dplyr::group_by(CLUST) %>%
    dplyr::filter(sum(MAX != 0) >= 5) %>%
    dplyr::mutate(CLUST_MEDIAN = median(MAX)) %>%
    dplyr::ungroup() %>%
    dplyr::filter((MAX_N >= 10 & MAX_MONTHS >= 3) | MAX >= CLUST_MEDIAN)

  swe_max[[eco3]] <- ym

}

final_max <- dplyr::bind_rows(swe_max)

readr::write_csv(final_max, "data-raw/jesse_max_depths.csv")

final_max_group <- final_max %>%
  dplyr::group_by(CLUST) %>%
  dplyr::summarise(max = median(MAX, na.rm = TRUE),
                   month = round(median(MAX_MONTH, na.rm = TRUE)))

climate <- read.csv("data-raw/ghcnd_climate.csv") %>%
  dplyr::select(ID, TD, FFP, MCMT, MWMT, PPTWT, RH, MAT, D2C)

stations_clust <- stations %>%
  dplyr::left_join(., climate, by = "ID") %>%
  dplyr::group_by(CLUST) %>%
  dplyr::summarise(LATITUDE = mean(LATITUDE),
                   LONGITUDE = mean(LONGITUDE),
                   ELEVATION = mean(ELEVATION),
                   TD = mean(TD, na.rm = TRUE),
                   FFP = mean(FFP, na.rm = TRUE),
                   MCMT = mean(MCMT, na.rm = TRUE),
                   MWMT = mean(MWMT, na.rm = TRUE),
                   PPTWT = mean(PPTWT, na.rm = TRUE),
                   RH = mean(RH, na.rm = TRUE),
                   MAT = mean(MAT, na.rm = TRUE),
                   D2C = mean(D2C, na.rm = TRUE))


final_w_locations <- left_join(final_max_group, stations_clust, by = "CLUST")




readr::write_csv(final_w_locations, "data-raw/jesse_max_depths_w_loc.csv")

