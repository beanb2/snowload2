tabler_1980 <- function(h) {
  # snow_depth = h measured in meters. Returns kg/m^3
  158 * log10(h) + 376

  # Paper doesn't mention if it is log10 or log_e, I assume log10 because
  # there is another method that specifically uses ln
}
