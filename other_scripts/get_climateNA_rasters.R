# This file contains the code used to deal with the climateNA
# variables. There are three sections in the code:
#
# - Downloading ClimateNA grids. This was done as part of the
#   depth-to-load problem, I didn't rerun this code I just used
#   the data that was already stored on my computer
# - Transforming the ClimateNA grids to a simpler projection
# - Extracting values from the grid.



# Downloading ClimateNA grids ---------------------------------------------

dir.create('data-raw/climateNA')

RASTERS <- c(
  'TD.asc', 'FFP.asc', 'MCMT.asc', 'MWMT.asc', 'PPT_wt.asc',
  'RH.asc', 'MAT.asc'
)

URL <- 'http://www.cacpd.org/climate_normals/NA_NORM_6190_Bioclim_ASCII.7z'

download.file(URL, destfile = 'data-raw/climateNA/climateNA.7z')
arch <- archive::archive('data-raw/climateNA/climateNA.7z')

archive::archive_extract(
  archive = arch,
  dir = 'data-raw/climateNA/',
  file = RASTERS
)

prj_URL <- 'http://www.cacpd.org.s3.amazonaws.com/north_america/projection.prj'

download.file(prj_URL, destfile = 'data-raw/climateNA/TD.prj')

#  difference between mean temperature of the colderst month
#  and mean temperature of the warmest month, as a measure of
#  continentality (°C)
TD <- raster::raster(
  'data-raw/climateNA/TD.asc'
)

# length of the frost-free period
FFP <- raster::raster(
  'data-raw/climateNA/FFP.asc',
  crs = raster::projection(TD)
)

# mean temperature of the coldest month (°C)
MCMT <- raster::raster(
  'data-raw/climateNA/MCMT.asc',
  crs = raster::projection(TD)
)

# mean temperature of the warmest month (°C)
MWMT <- raster::raster(
  'data-raw/climateNA/MWMT.asc',
  crs = raster::projection(TD)
)

# winter (Dec to Feb) precipitation (mm)
PPTWT <- raster::raster(
  'data-raw/climateNA/PPT_wt.asc',
  crs = raster::projection(TD)
)

# Relative Humidity
RH <- raster::raster(
  'data-raw/climateNA/RH.asc',
  crs = raster::projection(TD)
)

# Mean Annual Temperature
MAT <- raster::raster(
  'data-raw/climateNA/MAT.asc',
  crs = raster::projection(TD)
)

# Combining raster layers into a single raster brick
final_raster_brick <- raster::brick(
  list('TD' = TD,
       'FFP' = FFP,
       'MCMT' = MCMT,
       'MWMT' = MWMT,
       'PPTWT' = PPTWT,
       'RH' = RH,
       'MAT' = MAT)
)

# Save raster brick.
raster::writeRaster(
  final_raster_brick,
  file = 'data-raw/RObjects/climateNA.grd',
  overwrite = TRUE
)

# test_brick <- brick(
#   'data-raw/rasters/climateNA.grd'
# )

unlink('data-raw/climateNA', recursive = TRUE)
rm(
  list = ls()
)
gc()


# Reprojecting ClimateNA grids --------------------------------------------

library(raster)
library(sp)

# The file path is the local path on my computer where I
# stored the climateNA grids that were downloaded above.
climateNA_grd <- raster::brick(
  '~/Documents/GitHub/rpackages/swecnvt/data-raw/RObjects/climateNA.grd'
)

# This is the elevation grid raster that Brennan placed
# in the box folder:
# National Snow Project/data/elevation/final_raster.grd (and gri)
elevation_grd <- raster::raster('data/final_raster.grd')

# The extent given was manually determined by checking the
# max/min lat/lon values in the climateNA grid. If you
# don't supply limits, the final raster projection will have
# the extent equial to the entire world, which takes more
# memory than necessary (and slows down the reprojection
# process).
r <- raster(xmn = -130, xmx = -59.5, ymn = 25, ymx = 60)
crs(r) <- CRS(proj4string(elevation_grd))
res(r) <- res(elevation_grd)

climateNA_grd <- projectRaster(
  from climateNA_grd,
  to = r
)

raster::writeRaster(climateNA_grd, 'data/transClimate1.grd')


# Extract Raster Values ---------------------------------------------------

loads <- read.csv('~/Documents/GitHub/rpackages/snowload/data/national_50year_gev_rr100_20200602.csv',
                  stringsAsFactors = FALSE) %>%
  mutate(EVENT50 = 20.89 * EVENT50) %>%
  mutate(
    clust1 = cluster_stations(
      LONGITUDE, LATITUDE, ELEVATION,
      dist_adj = 30,
      elev_adj = 300,
      h = 2
    ),
    clust2 = cluster_stations(
      LONGITUDE, LATITUDE, ELEVATION,
      dist_adj = 50,
      elev_adj = 500,
      h = 2
    )
  )

ClimateNA <- raster::brick('data/transClimate1.grd')
coordinates(loads) <- c("LONGITUDE", 'LATITUDE')
proj4string(loads) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

loads <- sp::spTransform(loads, CRSobj = CRS(proj4string(ClimateNA)))

# Get all values that are over cell
extracted <- raster::extract(ClimateNA, loads,
                             df = TRUE)


# Some values are missing, so set these aside and try using
# a buffer. Let the value be missing average of values
# within 500 m
missing_sp <- loads[extracted[is.na(extracted$RH), 'ID'], ]
extracted_missing1 <- raster::extract(ClimateNA, missing_sp,
                                      df = TRUE,
                                      buffer = 500,
                                      fun = mean)

# Save the calculated missing values
extracted[is.na(extracted$RH),
          c('TD', 'FFP', 'MCMT', 'MWMT',
            'PPTWT', 'RH', 'MAT')] <- dplyr::select(extracted_missing1, -ID)

# Some values are STILL missing, so set these aside and try
# again. Let the value be missing average of values
# within 1000 m
missing_sp <- loads[extracted[is.na(extracted$RH), 'ID'], ]
extracted_missing2 <- raster::extract(ClimateNA, missing_sp,
                                      df = TRUE,
                                      buffer = 1000,
                                      fun = mean)

# Save the calculated missing values
extracted[is.na(extracted$RH),
          c('TD', 'FFP', 'MCMT', 'MWMT',
            'PPTWT', 'RH', 'MAT')] <- dplyr::select(extracted_missing2, -ID)


# Some values are STILL missing, so set these aside and try
# again. Let the value be missing average of values
# within 1500 m
missing_sp <- loads[extracted[is.na(extracted$RH), 'ID'], ]
extracted_missing3 <- raster::extract(ClimateNA, missing_sp,
                                      df = TRUE,
                                      buffer = 1500,
                                      fun = mean)

# Save the calculated missing values
extracted[is.na(extracted$RH),
          c('TD', 'FFP', 'MCMT', 'MWMT',
            'PPTWT', 'RH', 'MAT')] <- dplyr::select(extracted_missing3, -ID)


# only one missing point left, so set these aside and try
# again. Using cutoffs of 1000 m, 8000 was the first to
# work.
missing_sp <- loads[extracted[is.na(extracted$RH), 'ID'], ]
extracted_missing4 <- raster::extract(ClimateNA, missing_sp,
                                      df = TRUE,
                                      buffer = 8000,
                                      fun = mean)

# Save the calculated missing values
extracted[is.na(extracted$RH),
          c('TD', 'FFP', 'MCMT', 'MWMT',
            'PPTWT', 'RH', 'MAT')] <- dplyr::select(extracted_missing4, -ID)

# now there are no missing values, so save this new dataset.
loads <- dplyr::bind_cols(
  loads@data,
  as.data.frame(extracted),
  as.data.frame(loads@coords)
)
