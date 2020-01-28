pomeroy_gray_1995 <- function(h) {
  # h = snow depth, measured in M. Outputs density measured in kg/m^3
  488 - (204.7 / h) * (1 - exp(-h / 0.673))
}
