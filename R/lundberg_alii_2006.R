lundberg_alii_2006 <- function(h) {
  # Returns results in kg/m^3, h = snow_depth in meters. See avanzi et al, 2015
  ifelse(h <= 2,
         148 + 105 * h,
         358)
}
