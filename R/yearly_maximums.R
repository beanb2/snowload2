#' Get the maximum values by "water year".
#'
#' This function will group by id variable(s) and return the maximum values
#' per "water year". "Water year" is defined as a 12 month period  that begins
#' October 1 and ends September 30. The year is the calender year in which the
#' period ends.
#'
#' If a certain value type is to be prioritized, the value_type column should
#' be specified along with the specific value_type(s) to prioritize (e.g.
#' value_type = ELEMENT, prioritize = "WESD"). Values that have priority will
#' be chosen over non-prioritized values for a given year (e.g. if the maximum
#' value is for the year is 300 for type "SNWD" but the prioritized "WESD"
#' has max value 250, 250 will be chosen as the maximum).
#'
#' @param station_data A data frame with station snow data.
#' @param id Column containing station ID's, may include other grouping
#'   variables.
#' @param date Column containing the observation date.
#' @param value Column containing the observations to maximize
#' @param value_type Optional column containing the value types.
#' @param prioritize Optional value_type(s) to prioritize when selecting.
#' @param prioritize_threshold Optional named list with requirements to use
#' the prioritized value containing \emph{n} - the minimum number of
#' observations and \emph{months} - the minimum number of months that must have
#' a value.
#'
#' @return A data.frame where each row is a maximum for a given id and year:
#'   \describe{
#'     \item{}{\emph{id} - Given id column and possibly other grouping columns.}
#'     \item{}{\emph{YEAR} - The "water years" of the given dates.}
#'     \item{}{\emph{MAX} - The maximum of the given values.}
#'     \item{}{\emph{MAX_N} - Number of values used to find the max.}
#'     \item{}{\emph{MAX_MONTHS} - Number of unique months with values.}
#'     \item{}{\emph{MAX_MONTH} - Month in which the maximum was found.}
#'     \item{}{\emph{PRIORITIZED} - Only applies when \emph{prioritize} is not
#'     null. True when max value is a prioritized value, false otherwise.}
#'   }
#'
#' @examples
#' x <- get_station_data(ghcnd_stations$ID[10000],
#'                       "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/")
#' yearly_maximums(x, ID, DATE, VALUE)
#'
#'
#' @export
yearly_maximums <- function(station_data, id, date, value, ratio = NULL,
                            value_type = NULL, prioritize = NULL,
                            prioritize_threshold = list(n = 10, months = 3)) {
  # Prepare column names
  #=============================================================================
  id <- dplyr::enquo(id)
  date <- dplyr::enquo(date)
  value <- dplyr::enquo(value)
  value_type <- dplyr::enquo(value_type)
  if(!is.null(ratio)){
    ratio <- dplyr::enquo(ratio)
  }else{
    ratio <- 1
  }

  # Find maximums
  #=============================================================================
  station_data <- dplyr::mutate(
    station_data,
    MONTH = as.numeric(lubridate::month(!! date)),
    YEAR = as.numeric(lubridate::year(!! date)),
    YEAR = dplyr::if_else(MONTH > 9, YEAR + 1, YEAR)
  )

  if (!is.null(value_type) && is.null(prioritize)){
    station_data <- dplyr::group_by(station_data, !! id, !! value_type, YEAR)
  }else{
    station_data <- dplyr::group_by(station_data, !! id, YEAR)
  }

  # Add string of months.
  MONTH_NAMES <- dplyr::summarise(station_data,
                                  MONTH_NAMES = paste(unique(MONTH), collapse = ":"))

  if (!is.null(value_type) && !is.null(prioritize)) {
    station_data <- dplyr::filter(
      station_data,
      dplyr::if_else(rep(sum(!! value_type %in% prioritize) >=
                           prioritize_threshold$n &&
                           length(unique(MONTH[!! value_type %in% prioritize])) >=
                           prioritize_threshold$month &&
                           max((!! value)[!! value_type %in% prioritize]) > 0,
                         n()),
                     true = !! value_type %in% prioritize,
                     false = TRUE)
    )

    tmax_final <- dplyr::summarise(station_data,
                                   MAX = max(!! value),
                                   MAX_N = n(),
                                   MAX_MONTH = MONTH[which(!! value == MAX)][[1]],
                                   MAX_MONTHS = length(unique(MONTH)),
                                   RATIO = (!! ratio)[which(!! value == MAX)])[[1]],
                                   PRIORITIZED = (!! value_type)[which.max(!! value)] %in%
                                     prioritize)
  } else {
    tmax_final <- dplyr::summarise(station_data,
                                   MAX = max(!! value),
                                   MAX_N = n(),
                                   MAX_MONTH = MONTH[which(!! value == MAX)][[1]],
                                   MAX_MONTHS = length(unique(MONTH)),
                                   RATIO = (!! ratio)[which(!! value == MAX)])[[1]])
  }



  tmax_final <- dplyr::left_join(tmax_final, MONTH_NAMES)
}

