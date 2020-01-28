pomeroy_alii_1998 <- function(h) {
  # h = snowdepth measured in M. Outputs density kg/M^2
  450 - (204.7 / h) * (1 - exp(-h / 0.673))
}
