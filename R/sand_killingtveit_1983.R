sand_killingtveit_1983_1 <- function(h) {
  # snow_depth = h measured in meters, outputs density in kg/m^3
  ifelse(
    h <= 1.7,
    27 * h + 358,
    404
  )
}

sand_killingtveit_1983_2 <- function(h) {
  # snow_depth = h measured in meters, outputs density in kg/m^3
  ifelse(
    h <= 1.7,
    72 * h + 275,
    397
  )
}
