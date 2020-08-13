# Updated correlation analysis for Colorado
library(tidyverse)

co_stations <- snowload2::ghcnd_stations %>%
  dplyr::filter(STATE %in% c("CO"))

# Download new Colorado data, include ALL available variables.
co_temp <- snowload2::get_state_data(states = "CO",
                                     source = "data-raw/ghcnd_all/",
                                     elem = c("SNWD", "WESD", "PRCP", "SNOW",
                                              "TMAX", "TMIN"))

# Only interested in stations with data
co_stations <- co_stations %>%
  dplyr::filter(ID %in% co_temp$ID) %>%
  dplyr::select(ID, LATITUDE, LONGITUDE, ELEVATION, STATE, NAME, ECO3)

# Fill in missing elevation data for one station
temp <- which(is.na(co_stations$ELEVATION))

for(i in temp){
  co_stations$ELEVATION[i] <-
    as.numeric(rgbif::elevation(latitude = co_stations$LATITUDE[i],
                     longitude = co_stations$LONGITUDE[i],
                     username = Sys.getenv("GEONAMES_USER"))["elevation_geonames"])
}

# Functions
#===============================================================================
# Use station locations to find close pairs of stations
close_station_pairs <- function(stations, max_dist, max_delev) {
  pairs <- list()
  for (i in 1:(nrow(stations) - 1)) {
    delev <- c(rep(Inf, i),
               abs(stations$ELEVATION[-(1:i)] - stations$ELEVATION[i]))
    keep <- delev <= max_delev
    dist <- rep(Inf, length(keep))
    dist[keep] <- fields::rdist.earth(matrix(c(stations$LONGITUDE[keep],
                                               stations$LATITUDE[keep]),
                                             ncol = 2),
                                      matrix(c(stations$LONGITUDE[i],
                                               stations$LATITUDE[i]),
                                             ncol = 2),
                                      miles = FALSE)
    keep <- keep & dist <= max_dist
    pairs[[i]] <- data.frame(
      ID1 = rep(stations$ID[i], sum(keep)),
      ID2 = stations$ID[keep],
      AVE_LON = (stations$LONGITUDE[keep] + stations$LONGITUDE[i]) / 2,
      AVE_LAT = (stations$LATITUDE[keep] + stations$LATITUDE[i]) / 2,
      AVE_ELEV = (stations$ELEVATION[keep] + stations$ELEVATION[i]) / 2,
      DIST = dist[keep],
      DELEV = delev[keep],
      stringsAsFactors = FALSE
    )
  }
  return(pairs)
}

# Use close pairs to find similarity between station pairs
add_mse <- function(station_pairs, measurements) {
  analysis <- measurements %>%
    dplyr::filter(ID == first(station_pairs$ID1)) %>%
    dplyr::rename(MAIN_ID = ID, MAIN_VALUE = VALUE) %>%
    dplyr::inner_join(measurements %>% dplyr::filter(ID %in% station_pairs$ID2),
                      by = c("DATE" = "DATE", "ELEMENT")) %>%
    dplyr::filter(MAIN_VALUE != 0 & VALUE != 0 | !is.element(ELEMENT, c("SNWD", "WESD"))) %>%
    dplyr::group_by(ID, ELEMENT) %>%
    dplyr::summarise(N = dplyr::n(),
                     pcorr = cor(MAIN_VALUE, VALUE))
  station_pairs %>%
    dplyr::left_join(analysis, by = c("ID2" = "ID")) %>%
    na.omit()
}
#===============================================================================


pairs_full <- close_station_pairs(co_stations, 30, 300) %>%
  lapply(add_mse, co_temp) %>%
  dplyr::bind_rows()

save(pairs_full, file = "data-raw/pairs-full.R")

load("../snowload2/data-raw/pairs-full.R")
pairs_sub <- dplyr::filter(pairs_full, N >= 30)

pairs_sub$distcut <- cut(pairs_sub$DIST, c(0, 2.5, 5, 10, 20, 30))
pairs_sub <- pairs_sub[!is.na(pairs_sub$distcut), ]

ggplot(pairs_sub, aes(x = DELEV, y = pcorr, color = AVE_ELEV)) +
  stat_binhex() +
  geom_smooth() +
  facet_wrap(ELEMENT ~ distcut, ncol = 5)
