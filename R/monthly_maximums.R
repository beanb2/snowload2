#' Get the maximum values by month.
#'
#' This function will group by id variable(s) and return the maximum values
#' per month. "Water year" is defined as a 12 month period  that begins
#' October 1 and ends September 30. The year is the calender year in which the
#' period ends.
#'
#' @param station_data A data frame with station snow data.
#' @param id Column containing station ID's, may include other grouping
#'   variables.
#' @param date Column containing the observation date.
#' @param value Column containing the observations to maximize
#' @param value_type Optional column containing the value types. If a value_type
#' column is specified, maximums will be found for each  unique value_type.
#'
#' @return A data.frame where each row is a maximum for a given id and month.
#' Columns will be the same as station_data input with the addition of:
#'   \describe{
#'     \item{}{\emph{MONTH} - Month for a given maximum.}
#'     \item{}{\emph{YEAR} - The "water years" of the given dates.}
#'     \item{}{\emph{MAX_N} - Number of values used to find the max.}
#'   }
#'
#' @seealso
#'   \code{\link{yearly_maximums}} - get maxes by year.
#'
#' @examples
#' x <- get_station_data(ghcnd_stations$ID[10000],
#'                       "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/")
#' monthly_maximums(x, ID, DATE, VALUE)
#'
#'
#' @export
monthly_maximums <- function(station_data, id, date, value, value_type) {

  # Prepare data
  #=============================================================================
  mm <- dplyr::mutate(
    station_data,
    MONTH = as.numeric(lubridate::month({{date}})),
    YEAR = as.numeric(lubridate::year({{date}})),
    YEAR = dplyr::if_else(MONTH > 9, YEAR + 1, YEAR)
  )

  if (!missing(value_type)){
    mm <- dplyr::group_by(mm, {{id}}, {{value_type}}, YEAR, MONTH)
  }else{
    mm <- dplyr::group_by(mm, {{id}}, YEAR, MONTH)
  }

  # Calculate maximums
  #=============================================================================
  mm <- dplyr::mutate(mm, MAX_N = dplyr::n())

  mm <- dplyr::arrange(mm, dplyr::desc({{value}}))

  mm <- dplyr::slice(mm, 1)

  mm <- dplyr::ungroup(mm)

  return(mm)
}

