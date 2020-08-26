# This script prepares the GHCND data gridded climate information.
# Created: 7-7-2020
`%>%` <- magrittr::`%>%`

# Step 1: Read in Data and Prepare Files:
#=============================================================================
# Read in the ghcnd_stations (get_ghcnd_stations should have been
# run prior to this script and reinstalled with the new dataset).
stations <- as.data.frame(snowload2::ghcnd_stations)

# Read in the prism grids
prism_final <- raster::brick("C:/Users/Brennan/Desktop/snowload_data_master/final_prism_grid_800m_new_elevation.grd")
prism_c <- raster::brick("C:/Users/Brennan/Desktop/snowload_data_master/final_prism_grid_4km.grd")

# Read in the climateNA info (4km resolution)
climateNA_final <- raster::brick("C:/Users/Brennan/Desktop/snowload_data_master/climateNA.grd")

# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
shoreline <- rgdal::readOGR(dsn = "data-raw/gshhg-shp-2.3.7/GSHHS_shp/c",
                            layer = "GSHHS_c_L1")
shoreline2 <- rgdal::readOGR(dsn = "data-raw/gshhg-shp-2.3.7/GSHHS_shp/c",
                             layer = "GSHHS_c_L2")

# Crop according to approximate coordinate extent of the United States
# (obtained via Google Earth).
# The extent object allows us to only retain pieces of the shapefiles that
# reside within the box defined by the extent object
# (minLon, maxLon, minLat, maxLat)
shoreline_crop <- raster::crop(shoreline,
                               raster::extent(c(-180, -60, 25, 71.5)))
shoreline2_crop <- raster::crop(shoreline2,
                                raster::extent(c(-180, -60, 25, 71.5)))

# Reproject to the PRISM coordinate reference system.
shoreline_crop <- sp::spTransform(shoreline_crop,
                                  CRSobj = sp::CRS(raster::projection(prism_final)))
shoreline2_crop <- sp::spTransform(shoreline2_crop,
                                  CRSobj = sp::CRS(raster::projection(prism_final)))

# My guess is that area is provided in km^2. However, knowing this is not
# necessary as long as we can retain only the largest lakes in the dataset.
# We are assuming that the great lakes count as "coasts" in the distance to
# coast calculation.

# Only keeps the largest lakes (i.e. the great lakes)
# boxplot(shoreline2_crop$area, ylim = c(0, 50000))
shoreline2_crop <- shoreline2_crop[shoreline2_crop$area > 20000, ]

# Only keep the North American continent (which has the second largest area,
# with Eurasia having the largest area)
shoreline_crop <- shoreline_crop[shoreline_crop$area > 1e07 &
                                   shoreline_crop$area < 4e07, ]

# Confirm that the final shapefiles look as expected.
sp::plot(shoreline_crop)
sp::plot(shoreline2_crop, col = "blue", add = TRUE)
#=============================================================================

# Step 2: Obtain distance to coast calculations.
#=============================================================================

# Create a spatial data frame
sp::coordinates(stations) <- c("LONGITUDE", "LATITUDE")
sp::proj4string(stations) <- sp::proj4string(prism_final)

# Calculate the shortest distance to a boundary from either object.
# Returns distances in meters.
Sys.time()
test <- geosphere::dist2Line(p = stations, line = shoreline_crop)
test2 <- geosphere::dist2Line(p = stations, line = shoreline2_crop)
Sys.time()

test_final <- test[, 1]
test_final[test_final > test2[, 1]] <- test2[test_final > test2[, 1], 1]

# Retain only the shortest distance for each location.
stations$dist2coast <- test_final
#==============================================================================

# Step 3: Obtain PRISM Variables
#==============================================================================
prism_extract <- raster::extract(prism_final, stations,
                                 method = "bilinear", df = TRUE)

# Only retain the relevant variables in a consistent order.
prism_extract <- prism_extract %>%
  dplyr::select(PPTWT, MCMT, MWMT, TD, MAT, ELEVATION_PRISM = ELEVATION)


#==============================================================================

# Step 3: Obtain PRISM Variables
#==============================================================================


# Step 5: Add ClimateNA variables
#==============================================================================
# Use bilinear interpolation to smooth out extraction.
temper <- raster::extract(climateNA_final, stations,
                          method = "bilinear", df = TRUE)
temper <- temper %>%
  dplyr::select(PPTWT, MCMT, MWMT, TD, MAT) %>%
  dplyr::mutate(ELEVATION_PRISM = NA)

# Replace missing PRISM predictions with climateNA predictions.
prism_extract2 <- prism_extract
prism_extract2[is.na(prism_extract$ELEVATION_PRISM), ] <- temper[is.na(prism_extract$ELEVATION_PRISM), ]

# For locations falling outside the grid, use a 10km buffer for 4km PRISM.
# Anything that still does not match gets thrown out.
# (4km grid used for speed since we are already averaging)
stations_sub <- stations[is.na(prism_extract2$PPTWT), ]
buffer_check <- raster::extract(prism_c, stations_sub,
                          buffer = 5000, fun = mean)

# Replace missing values with the buffer values.
prism_extract3 <- prism_extract2
prism_extract3[is.na(prism_extract2$PPTWT), ] <- buffer_check[is.na(prism_extract2$PPTWT), ]

ghcnd_stations_climate <- dplyr::bind_cols(as.data.frame(stations),
                                           as.data.frame(temper[, -1]))
ghcnd_stations_climate <- dplyr::select(ghcnd_stations_climate,
                                        ID, dist2coast, TD, FFP, MCMT,
                                        MWMT, PPTWT, RH, MAT)
#==============================================================================


