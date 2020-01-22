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
ghcnd_stations <- ghcnd_stations %>% filter(!is.na(STATE))
usethis::use_data(ghcnd_stations)








