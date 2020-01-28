tabler_alii_1990 <- function(h) {
  # h = snow depth, measured in M. Outputs density measured in kg/m^3
  522 - (204.7 / h) * (1 - exp(-h / 0.673))
}
