RMCD <- function(snow_depth) {
  ifelse(snow_depth <= 22,
         0.90 * snow_depth,
         2.36 * snow_depth - 31.9)
}
