# This is an R-file where I will keep the vectorized "Strums" equation
#
# Created: Jan 23, 2020
# Author: Jesse Wheeler

strums <- function(snow_depth, date, climate) {

  VALUE = snow_depth
  DOYA = date
  climate_row <- dplyr::case_when(
    climate == 'Alpine' ~ 1,
    climate == 'Maritime' ~ 2,
    climate == 'Prarie' ~ 3,
    climate == 'Tundra' ~ 4,
    climate == 'Taiga' ~ 5,
    TRUE ~ 6
  )

  params <- data.frame(
    pmax = c(0.5975,	0.5979,	0.594, 0.363, 0.217,
             (0.5975 + 0.5979 + 0.594)/3),
    po = c(0.2237, 0.2578, 0.2332,	0.2425,	0.217,
           (0.2237 + 0.2578 + 0.2332)/3),
    k1 = c(0.0012, 0.001, 0.0016, 0.0029, 0,
           (0.0012 + 0.001 + 0.0016)/3),
    k2 = c(0.0038,	0.0038, 0.0031, 0.0049, 0,
           (0.0038 + 0.0038 + 0.0031)/3)
  )

  pmax <- params[climate_row, 'pmax']
  po <- params[climate_row, 'po']
  k1 <- params[climate_row, 'k1']
  k2 <- params[climate_row, 'k2']

 ((pmax - po) * (1 - exp((-k1 * snow_depth) - (k2 * date))) + po)
}
