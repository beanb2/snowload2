marchand_killingtveit_2004 <- function(h) {
  # h = depth of snow measured in M. Outputs density in kg/m^3
  ifelse(
    2.5692 * h + 331.81 > 390,
    390,
    2.5692 * h + 331.81 > 390
  )
}
