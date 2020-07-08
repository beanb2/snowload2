library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Set constants
#===============================================================================
names <- c("ID", "LATITUDE", "LONGITUDE", "ELEVATION", "STATE", "NAME",
           "GSN_FLAG", "HCN_CRN_FLAG", "WMO_ID")
widths <- c(11, 9, 10, 7, 3, 31, 4, 4, 6)

# Read in the final grid (which will dictate all projections)
elev_grd <- raster::brick("C:/Users/Brennan/Desktop/snowload_data_master/climate_elevation_grd")

# Collect and alter data
#===============================================================================
ghcnd_stations <- read_fwf(
  "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt",
  fwf_widths(widths, names),
  col_types = cols(HCN_CRN_FLAG = col_character())
)

# Map stations with no id
# from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() +
  geom_point(data = ghcnd_stations %>% filter(is.na(STATE)),
             aes(x = LONGITUDE, y = LATITUDE)) +
  theme_bw()

# Based on above map, remove stations with no state
ghcnd_stations <- ghcnd_stations %>%
  filter(!is.na(STATE),
         ELEVATION != -999.9,
         LATITUDE != -999.9,
         LONGITUDE != -999.9)

# Create spatial version of the stations.
ghcnd_stations_sp <- ghcnd_stations
sp::coordinates(ghcnd_stations_sp) <- c("LONGITUDE", "LATITUDE")
sp::proj4string(ghcnd_stations_sp) <- sp::proj4string(elev_grd)



# Add eco regions
#===============================================================================
# Shapefile obtained from
#
eco3 <- rgdal::readOGR(dsn = "data-raw/eco_regions",
                       layer = "NA_CEC_Eco_Level3")
eco3 <- sp::spTransform(eco3, sp::CRS(sp::proj4string(elev_grd)))

# Determine the largest polygon for each area and make sure we don't
# filter at a value larger than the smallest of these.
eco3_data <- eco3@data %>%
  dplyr::group_by(NA_L3CODE) %>%
  summarize(maxarea = max(Shape_Area))

# This subsetting removes the following eco-regions (all of which are
# inconsequential):
# 14.6.2, 2.2.6, 15.3.1, 15.5.1, 13.4.1
# This code makes the assumption that 90% of the polygons are too small
# to be given serious consideration.
eco3_sub <- eco3[eco3$Shape_Area > quantile(eco3$Shape_Area, 0.9), ]

# eco3_simp1 <- rgeos::gSimplify(eco3_sub, tol = 0.1, topologyPreserve = TRUE)
eco3_simp2 <- rgeos::gSimplify(eco3_sub, tol = 0.1)

# Ironically, the non-topology preserving eco regions are considered a valid
# topology while the topology preserving version is not. We will stick with
# the simpler version.
eco3_final <- SpatialPolygonsDataFrame(eco3_simp2, data = eco3_sub@data)

# Use the full eco-regions for the majority of the stations
station_eco <- sp::over(ghcnd_stations_sp, eco3)
ghcnd_stations_sp$ECO3 <- station_eco$NA_L3CODE
# Only worry about assigning stations that are within roughly 2 degrees of
# the outer shell of eco-regions.
eco3_buffer <- rgeos::gBuffer(eco3_final, width = 2)
station_eco_buffer <- sp::over(ghcnd_stations_sp, eco3_buffer)

ghcnd_stations_sp2 <- ghcnd_stations_sp[!is.na(station_eco_buffer) |
                                          !is.na(ghcnd_stations_sp$ECO3), ]

# For the missing values, we will use the simplified eco-regions
station_missing <- ghcnd_stations_sp2[is.na(ghcnd_stations_sp2$ECO3), ]

for(i in 1:length(station_missing)){
tc <- sp::coordinates(station_missing[i, ])
temp <- raster::crop(eco3_final, raster::extent(max(tc[1] - 3, -180), min(tc[1] + 3, 180),
                                                max(tc[2] - 3, -90), min(tc[2] + 3, 90)))
tdist <- geosphere::dist2Line(station_missing[i, ], temp)
station_missing$ECO3[i] <- temp$NA_L3CODE[tdist[, "ID"]]
}

ghcnd_stations_sp2$ECO3[is.na(ghcnd_stations_sp2$ECO3)] <- station_missing$ECO3

ghcnd_stations <- as.data.frame(ghcnd_stations_sp2)

# Add state maximums
#===============================================================================
state_max <- read_csv("https://www.ncdc.noaa.gov/extremes/scec/records.csv") %>%
  dplyr::filter(Element == "All-Time Maximum Snow Depth") %>%
  dplyr::left_join(data.frame(State = state.name, Abb = state.abb)) %>%
  dplyr::filter(!is.na(Abb)) %>%
  dplyr::select(STATE = Abb, STATE_MAX = Notes) %>%
  dplyr::distinct() %>%
  dplyr::mutate(STATE_MAX = STATE_MAX * 25.4)# convert inches to mm

ghcnd_stations <- ghcnd_stations %>%
  left_join(state_max)



# Add counties
#===============================================================================
counties <- sf::st_as_sf(maps::map("county", plot = FALSE, fill = TRUE)) %>%
  tidyr::separate(ID, into = c("FULL_STATE", "COUNTY"), sep = ",") %>%
  dplyr::mutate(FULL_STATE = str_to_title(FULL_STATE),
         COUNTY = str_to_title(COUNTY)) %>%
  dplyr::left_join(data.frame(FULL_STATE = as.character(state.name),
                       STATE = as.character(state.abb))) %>%
  dplyr::mutate(STATE = if_else(FULL_STATE == "District Of Columbia",
                         "DC", as.character(STATE))) %>%
  dplyr::select(-FULL_STATE)


states <- c(
  "AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "ID", "IL", "IN",
  "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
  "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
  "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

state_county_info <- list()
for (state in states) {
  print(paste("State =", state))
  state_counties <- counties %>% dplyr::filter(STATE == state)

  ghcnd_cnty <- sf::st_as_sf(ghcnd_stations %>%
                               dplyr::filter(STATE == state) %>%
                               dplyr::select(ID, STATE, LATITUDE, LONGITUDE),
                             coords = c("LONGITUDE", "LATITUDE"),
                             crs = 4326) %>%
    dplyr::mutate(index = as.integer(sf::st_intersects(geometry, state_counties)))

  for (i in 1:nrow(ghcnd_cnty)) {
    if (is.na(ghcnd_cnty$index[i])) {
      x <- sf::st_distance(ghcnd_cnty[i,], state_counties)
      index <- which.min(x)
      ghcnd_cnty$index[i] <- index
    }
  }

  state_county_info[[state]] <- ghcnd_cnty %>%
    dplyr::mutate(COUNTY = state_counties$COUNTY[index]) %>%
    st_set_geometry(NULL) %>%
    dplyr::select(ID, COUNTY)
}



ghcnd_stations <- ghcnd_stations %>%
  dplyr::left_join(dplyr::bind_rows(state_county_info))


# Save data in package
#===============================================================================
usethis::use_data(ghcnd_stations, overwrite = TRUE)




# County maxes
#===============================================================================
load("other_scripts/county_max_data.RData")

county_maxes <- county_max_data %>%
  tidyr::pivot_longer(c(-state, -county.name)) %>%
  dplyr::mutate(name = factor(name, levels = c("one.day.value", "two.day.value",
                                        "three.day.value", "four.day.value",
                                        "five.day.value", "six.day.value",
                                        "seven.day.value", "eight.day.value",
                                        "nine.day.value", "ten.day.value")),
         DAYS = as.numeric(name)) %>%
  dplyr::select(STATE = state, COUNTY = county.name, DAYS, COUNTY_MAX = value)


usethis::use_data(county_maxes, overwrite = TRUE)














