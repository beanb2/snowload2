# This script prepares the GHCND data gridded climate information.
# Created: 7-7-2020

# Read in the ghcnd_stations (get_ghcnd_stations should have been
# run prior to this script and reinstalled with the new dataset).
stations <- as.data.frame(snowload2::ghcnd_stations)

# Create a spatial data frame
sp::coordinates(stations) <- c("LONGITUDE", "LATITUDE")

# The first file contains oceanic shorelines. The second file contains
# prominent lakes and rivers.
shoreline <- rgdal::readOGR(dsn = "data-raw/gshhg-shp-2.3.7/GSHHS_shp/c",
                            layer = "GSHHS_c_L1")
shoreline2 <- rgdal::readOGR(dsn = "data-raw/gshhg-shp-2.3.7/GSHHS_shp/c",
                             layer = "GSHHS_c_L2")

# The shorelines are standard lat/lon coordinates, reproject the stations
# to this extent.
sp::proj4string(stations) <- sp::proj4string(shoreline)

# Crop according to approximate coordinate extent of the United States
# (obtained via Google Earth).
# The extent object allows us to only retain pieces of the shapefiles that
# reside within the box defined by the extent object
# (minLon, maxLon, minLat, maxLat)
shoreline_crop <- raster::crop(shoreline,
                               raster::extent(c(-180, -60, 25, 71.5)))
shoreline2_crop <- raster::crop(shoreline2,
                                raster::extent(c(-180, -60, 25, 71.5)))

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

# Calculate the shortest distance to a boundary from either object.
# Returns distances in meters.
test <- geosphere::dist2Line(p = stations, line = shoreline_crop)
test2 <- geosphere::dist2Line(p = stations, line = shoreline2_crop)

test_final <- test[, 1]
test_final[test_final > test2[, 1]] <- test2[test_final > test2[, 1], 1]

# Retain only the shortest distance for each location.
stations$dist2coast <- test_final
#==============================================================================


# Step 5: Add Climate NA variables
#==============================================================================
# This file path needs to be changed. The idea is that the high memory
# files will be stored in a separate folder on Box, and downloaded to the
# local machine should the need arise to recreate the datasets.
climateNA <- raster::brick("C:/Users/Brennan/Desktop/snowload_data_master/climateNA.grd")

try <- sp::spTransform(stations, sp::CRS(sp::proj4string(climateNA)))

# Use bilinear interpolation to smooth out extraction.
temper <- raster::extract(climateNA, try, method = "bilinear", df = TRUE)

# For locations falling outside the grid, use a 10km buffer. Anything short
# of 10km causes issues with the data match.
temper3 <- raster::extract(climateNA, try[is.na(temper$TD), ],
                          buffer = 10000, fun = mean)

# Replace missing values with the buffer values.
temper[is.na(temper$TD), -1] <- temper3

ghcnd_stations_climate <- dplyr::bind_cols(as.data.frame(stations),
                                           as.data.frame(temper[, -1]))
ghcnd_stations_climate <- dplyr::select(ghcnd_stations_climate,
                                        ID, dist2coast, TD, FFP, MCMT,
                                        MWMT, PPTWT, RH, MAT)

usethis::use_data(ghcnd_stations_climate)
#==============================================================================
