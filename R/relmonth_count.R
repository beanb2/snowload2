#' Count the number of months intersecting a window where
#'   unique months are provided as a concantenated string.
#'
#' @param string The string of months, with each month separated by a specfic token.
#' @param window The relevant window of months, given as a numeric vector.
#' @param split The month separator in each string.
#'
#' @return The number of months in the string that intersect the window.
#' @examples
#' x <- "4:5:6"
#' relmonth_count(x, window = c(2, 4, 6))
#'
#' @export
relmonth_count <- function(string, window, split = ":"){
  tsub <- strsplit(string, split = split)

  return(unlist(lapply(tsub, function(x)
    sum(!is.na(match(as.numeric(x), window))))))
}
