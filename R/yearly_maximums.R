#' Get the maximum values by "water year".
#'
#' This function will group by id variable(s) and return the maximum values
#' per "water year". "Water year" is defined as a 12 month period  that begins
#' October 1 and ends September 30. The year is the calender year in which the
#' period ends.
#'
#' @param station_data A data frame with station snow data.
#' @param id Column containing station ID's, may include other grouping
#'   variables.
#' @param date Column containing the observation date.
#' @param value Column containing the observations to maximize
#'
#' @return A data.frame where each row is a maximum for a given id and year:
#'   \describe{
#'     \item{}{\emph{id} - Given id column and possibly other grouping columns.}
#'     \item{}{\emph{YEAR} - The "water years" of the given dates.}
#'     \item{}{\emph{MAX} - The maximum of the given values.}
#'   }
#'
#' @examples
#' x <- get_station_data(ghcnd_stations$ID[10000],
#'                       "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/")
#' yearly_maximums(x, ID, DATE, VALUE)
#'
#'
#' @export
yearly_maximums <- function(station_data, id, date, value) {
  # Prepare column names
  #=============================================================================
  id <- dplyr::enquo(id)
  date <- dplyr::enquo(date)
  value <- dplyr::enquo(value)

  # Find maximums
  #=============================================================================
  station_data <- dplyr::mutate(
    station_data,
    YEAR = as.numeric(lubridate::year(!! date)),
    YEAR = if_else(lubridate::month(!! date) > 9, YEAR, YEAR + 1)
  )

  station_data <- dplyr::group_by(station_data, !! id, YEAR)

  dplyr::summarise(station_data, MAX = max(!! value))
}

