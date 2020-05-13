library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Set constants
#===============================================================================
names <- c("ID", "LATITUDE", "LONGITUDE", "ELEVATION", "STATE", "NAME",
           "GSN_FLAG", "HCN_CRN_FLAG", "WMO_ID")
widths <- c(11, 9, 10, 7, 3, 31, 4, 4, 6)

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



# Add eco regions
#===============================================================================
# Shapefile obtained from
#
eco3 <- sf::st_read("../NA_CEC_Eco_Level3/NA_CEC_Eco_Level3.shp")
eco3 <- sf::st_transform(eco3, crs = 4326) %>%
  filter(NA_L3CODE != "0.0.0")

ghcnd_sp <- sf::st_as_sf(ghcnd_stations %>%
                           select(ID, LATITUDE, LONGITUDE),
                         coords = c("LONGITUDE", "LATITUDE"),
                         crs = 4326) %>%
  mutate(index = as.integer(sf::st_intersects(geometry, eco3)))

for (i in 1:nrow(ghcnd_sp)) {
  if (is.na(ghcnd_sp$index[i])) {
    print(i)
    x <- sf::st_distance(ghcnd_sp[i,], eco3)
    index <- which.min(x)
    if (as.numeric(x[index]) < 300000) ghcnd_sp$index[i] <- index
  }
}

ghcnd_stations <- ghcnd_stations %>%
  left_join(ghcnd_sp %>%
              mutate(ECO3 = eco3$NA_L3CODE[index]) %>%
              st_set_geometry(NULL) %>%
              select(ID, ECO3))


# Map stations with no eco region
# from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() +
  geom_point(data = ghcnd_stations %>% filter(is.na(ECO3)),
             aes(x = LONGITUDE, y = LATITUDE)) +
  theme_bw()

# Based on above map, remove stations with no eco region
ghcnd_stations <- ghcnd_stations %>% filter(!is.na(ECO3))



# Add state maximums
#===============================================================================
state_max <- read_csv("https://www.ncdc.noaa.gov/extremes/scec/records.csv") %>%
  filter(Element == "All-Time Maximum Snow Depth") %>%
  left_join(data.frame(State = state.name, Abb = state.abb)) %>%
  filter(!is.na(Abb)) %>%
  select(STATE = Abb, STATE_MAX = Notes) %>%
  distinct() %>%
  mutate(STATE_MAX = STATE_MAX * 25.4)# convert inches to mm

ghcnd_stations <- ghcnd_stations %>%
  left_join(state_max)



# Save data in package
#===============================================================================
usethis::use_data(ghcnd_stations, overwrite = TRUE)












