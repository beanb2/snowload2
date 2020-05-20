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



# Add counties
#===============================================================================
counties <- sf::st_as_sf(maps::map("county", plot = FALSE, fill = TRUE)) %>%
  separate(ID, into = c("FULL_STATE", "COUNTY"), sep = ",") %>%
  mutate(FULL_STATE = str_to_title(FULL_STATE),
         COUNTY = str_to_title(COUNTY)) %>%
  left_join(data.frame(FULL_STATE = as.character(state.name),
                       STATE = as.character(state.abb))) %>%
  mutate(STATE = if_else(FULL_STATE == "District Of Columbia",
                         "DC", as.character(STATE))) %>%
  select(-FULL_STATE)


states <- c(
  "AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "ID", "IL", "IN",
  "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
  "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
  "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

state_county_info <- list()
for (state in states) {
  print(paste("State =", state))
  state_counties <- counties %>% filter(STATE == state)

  ghcnd_cnty <- sf::st_as_sf(ghcnd_stations %>%
                               filter(STATE == state) %>%
                               select(ID, STATE, LATITUDE, LONGITUDE),
                             coords = c("LONGITUDE", "LATITUDE"),
                             crs = 4326) %>%
    mutate(index = as.integer(sf::st_intersects(geometry, state_counties)))

  for (i in 1:nrow(ghcnd_cnty)) {
    if (is.na(ghcnd_cnty$index[i])) {
      x <- sf::st_distance(ghcnd_cnty[i,], state_counties)
      index <- which.min(x)
      ghcnd_cnty$index[i] <- index
    }
  }

  state_county_info[[state]] <- ghcnd_cnty %>%
    mutate(COUNTY = state_counties$COUNTY[index]) %>%
    st_set_geometry(NULL) %>%
    select(ID, COUNTY)
}



ghcnd_stations <- ghcnd_stations %>%
  left_join(bind_rows(state_county_info))


# Save data in package
#===============================================================================
usethis::use_data(ghcnd_stations, overwrite = TRUE)




# County maxes
#===============================================================================
load("other_scripts/county_max_data.RData")

county_maxes <- county_max_data %>%
  pivot_longer(c(-state, -county.name)) %>%
  mutate(name = factor(name, levels = c("one.day.value", "two.day.value",
                                        "three.day.value", "four.day.value",
                                        "five.day.value", "six.day.value",
                                        "seven.day.value", "eight.day.value",
                                        "nine.day.value", "ten.day.value")),
         DAYS = as.numeric(name)) %>%
  select(STATE = state, COUNTY = county.name, DAYS, COUNTY_MAX = value)


usethis::use_data(county_maxes, overwrite = TRUE)














