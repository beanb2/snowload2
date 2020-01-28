RMCD <- function(snow_depth) {
  # Returns lb/ft^2 density, snow depth in inches
  ifelse(snow_depth <= 22,
         0.90 * snow_depth,
         2.36 * snow_depth - 31.9)
}
