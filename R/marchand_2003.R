marchand_2003 <- function(h) {
  # h = snow depth, measured in M. Outputs density in kg/m^3
  ifelse(
    2.25,
    70 * h + 286,
    443
  )
}
