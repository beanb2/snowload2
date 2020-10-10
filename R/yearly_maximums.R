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
#' @param value Column containing the observations to maximize.
#' @param ratio Column containing the reatio used when converting maximums.
#' @param value_type Optional column containing the value types.
#' @param prioritize Optional value_type(s) to prioritize when selecting.
#' @param prioritize_threshold Optional named list with requirements to use
#' the prioritized value containing \emph{n} - the minimum number of
#' observations and \emph{months} - the minimum number of months that must have
#' a value.
#' @param monthly_maximums When \emph{station_data} is the output of
#' \code{\link{monthly_maximums}}, set this parameter to TRUE.
#'
#' @return A data.frame where each row is a maximum for a given id and year:
#'   \describe{
#'     \item{}{\emph{id} - Given id column and possibly other grouping columns.}
#'     \item{}{\emph{YEAR} - The "water years" of the given dates.}
#'     \item{}{\emph{MAX} - The maximum of the given values.}
#'     \item{}{\emph{MAX_N} - Number of values used to find the max.}
#'     \item{}{\emph{MAX_MONTHS} - Number of unique months with values.}
#'     \item{}{\emph{MAX_MONTH} - Month in which the maximum was found.}
#'     \item{}{\emph{RATIO} - Only applies when \emph{ratio} is not
#'     null. The ratio used when converting the maximum.}
#'     \item{}{\emph{PRIORITIZED} - Only applies when \emph{prioritize} is not
#'     null. True when max value is a prioritized value, false otherwise.}
#'   }
#'
#' @seealso
#'   \code{\link{monthly_maximums}} - get maxes by month.
#'
#' @examples
#' x <- get_station_data(ghcnd_stations$ID[10000],
#'                       "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/")
#' yearly_maximums(x, ID, DATE, VALUE)
#'
#'
#' @export
yearly_maximums <- function(station_data, id, date, value, ratio,
                            value_type, prioritize,
                            prioritize_threshold = list(n = 10, months = 3),
                            monthly_maximums = FALSE) {

  # Make sure station_data is output of monthly_maximums
  #=============================================================================
  if (!monthly_maximums) {
    station_data <- snowload2::monthly_maximums(
      station_data, {{id}}, {{date}}, {{value}}, {{value_type}}
    )
  }

  if(missing(ratio)){
    ratio <- 1
  }

  # Find maximums
  #=============================================================================
  if (!missing(value_type) && missing(prioritize)) {
    station_data <- dplyr::group_by(station_data, {{id}}, {{value_type}}, YEAR)
  }else{
    station_data <- dplyr::group_by(station_data, {{id}}, YEAR)
  }

  # Add string of months
  MONTH_NAMES <- dplyr::summarise(station_data,
                                  MONTH_NAMES = paste(unique(MONTH), collapse = ":"))

  if (!missing(value_type) && !missing(prioritize)) {
    station_data <- dplyr::filter(
      station_data,
      dplyr::if_else(rep(sum(({{value_type}} %in% prioritize) * MAX_N) >=
                           prioritize_threshold$n &&
                           length(unique(MONTH[{{value_type}} %in% prioritize])) >=
                           prioritize_threshold$month &&
                           max(({{value}})[{{value_type}} %in% prioritize]) > 0,
                         n()),
                     true = {{value_type}} %in% prioritize,
                     false = TRUE)
    )

    ym <- dplyr::summarise(station_data,
                           MAX = max({{value}}),
                           MAX_N = sum(MAX_N),
                           MAX_MONTH = MONTH[which({{value}} == MAX)][[1]],
                           MAX_MONTHS = length(unique(MONTH)),
                           RATIO = {{ratio}}[which({{value}} == MAX)][[1]],
                           PRIORITIZED = {{value_type}}[which.max({{value}})] %in%
                             prioritize)
  } else {
    ym <- dplyr::summarise(station_data,
                           MAX = max({{value}}),
                           MAX_N = sum(MAX_N),
                           MAX_MONTH = MONTH[which({{value}} == MAX)][[1]],
                           MAX_MONTHS = length(unique(MONTH)),
                           RATIO =  {{ratio}}[which({{value}} == MAX)][[1]])
  }



  tmax_final <- suppressMessages(dplyr::left_join(ym, MONTH_NAMES))
}

