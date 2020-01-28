#' Quality control function for removing rows from station data.
#'
#' A variety of quality control functionality is provided for cleaning
#' data gathered from the functions \code{\link{get_station_data}},
#' \code{\link{get_state_data}}, or \code{\link{get_year_data}}.
#'
#' @param station_data A data.frame in the format returned by the functions
#'   \code{\link{get_station_data}}, \code{\link{get_state_data}}, or
#'   \code{\link{get_year_data}}.
#' @param clean A character vector describing what cleaning steps to take.
#'   Default "all" will perform all cleaning steps.
#'   \describe{
#'     \item{}{\emph{"qflag"} - Removes all rows with a non-empty quality flag.
#'     See ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt, section 3
#'     for a description of the flag.}
#'   }
#'
#' @return A data.frame where each row is a measurement of a climate variable
#'   in elem for a single day and station. Columns are:
#'   \describe{
#'     \item{}{\emph{ID} - The station identification code. See the internal
#'     data set ghcnd_stations for station id's and descriptions.}
#'     \item{}{\emph{DATE} - The date of the measurement.}
#'     \item{}{\emph{ELEMENT} - The climate variable measured. See
#'     ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt, section 3  for
#'     a description of climate variables.}
#'     \item{}{\emph{VALUE} - The measurement of the ELEMENT on the given
#'     DATE.}
#'     \item{}{\emph{MFLAG} - A measurement flag. See
#'     ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt, section 3  for
#'     a description of the flag.}
#'     \item{}{\emph{QFLAG} - A quality flag. See
#'     ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt, section 3  for
#'     a description of the flag.}
#'     \item{}{\emph{SFLAG} - A source flag. See
#'     ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt, section 3  for
#'     a description of the flag.}
#'   }
#'
#' @seealso
#'   \code{\link{get_year_data}} - get data by year.
#'
#'   \code{\link{get_station_data}} - get data by station.
#'
#'   \code{\link{get_state_data}} - get data by state.
#'
#'
#' @export
clean_station_data <- function(station_data, clean = "all") {
  # Quality flag removal
  #=============================================================================
  if (clean == "all" || "qflag" %in% clean) {
    print("here")
    station_data <- dplyr::filter(station_data, QFLAG %in% c("", " ", NA))
  }

  return(station_data)
}
