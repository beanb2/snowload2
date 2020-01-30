# This file contains several small snow-depth to snow-density conversion
# methods. The majority of these methods were found in the paper:
#
#   Avanzi, Francesco, et al. On the Performances of Empirical Regressions
#     For the Estimation of Bulk Snow Density.
#
# The majority of these methods were fit to data from a specific location.
#
# Created: Jan 30, 2020
# Author: Jesse Wheeler
# Email: jesse.wheeler@aggiemail.usu.edu


lundberg_alii_2006 <- function(h) {
  # Returns results in kg/m^3, h = snow_depth in meters. See avanzi et al, 2015
  ifelse(h <= 2,
         148 + 105 * h,
         358)
}

Gavrilev_1965 <- function(h) {
  # Snow depth = h measured in meters, returns density kg/m^3
  180 * h + 90

  # NOTE: This method is incomplete but the original paper is in ?Russian?
}

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

marchand_2003 <- function(h) {
  # h = snow depth, measured in M. Outputs density in kg/m^3
  ifelse(
    2.25,
    70 * h + 286,
    443
  )
}

marchand_killingtveit_2004 <- function(h) {
  # h = depth of snow measured in M. Outputs density in kg/m^3
  ifelse(
    2.5692 * h + 331.81 > 390,
    390,
    2.5692 * h + 331.81 > 390
  )
}

tabler_alii_1990 <- function(h) {
  # h = snow depth, measured in M. Outputs density measured in kg/m^3
  522 - (204.7 / h) * (1 - exp(-h / 0.673))
}

tabler_1980 <- function(h) {
  # snow_depth = h measured in meters. Returns kg/m^3
  158 * log10(h) + 376

  # Paper doesn't mention if it is log10 or log_e, I assume log10 because
  # there is another method that specifically uses ln
}

pomeroy_gray_1995 <- function(h) {
  # h = snow depth, measured in M. Outputs density measured in kg/m^3
  488 - (204.7 / h) * (1 - exp(-h / 0.673))
}

pomeroy_alii_1998 <- function(h) {
  # h = snowdepth measured in M. Outputs density kg/M^2
  450 - (204.7 / h) * (1 - exp(-h / 0.673))
}

gustafsson_alii_2012 <- function(h) {
  # h = snowdepth measured in M, outputs kg/m^3
  352 + 75 * log(h)
}

elder_alii_1991 <- function(t) {
  # t = days from april 1st, outputs density in kg/m^3
  1.03 * t + 316
}
