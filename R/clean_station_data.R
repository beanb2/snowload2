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
#'     \item{}{\emph{"mpzero"} - Removes all rows with measurement flag of "P":
#'     missing, presumed zero.
#'     See ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt, section 3
#'     for a description of the flag.}
#'
#'     \item{}{\emph{"qflag"} - Removes all rows with a non-empty quality flag.
#'     See ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt, section 3
#'     for a description of the flag.}
#'
#'     \item{}{\emph{"month"} - Removes all rows with a month in the
#'     \emph{months} parameter. Typically summer months are removed.}
#'
#'     \item{}{\emph{"state_max"} - Removes snow depths that exceed the maximum
#'     depths for each state as listed in \emph{ghcnd_stations$STATE_MAX}.
#'     \emph{VALUE} must be unaltered. When \emph{ELEMENT} == "WESD", the
#'     maximum depth is multiplied by 5, then compared.}
#'
#'     \item{}{\emph{"county_max"} - Removes snow depths that exceed the maximum
#'     depths over time for each county as listed in
#'     \emph{county_maxes}. \emph{VALUE} must be unaltered. When \emph{ELEMENT}
#'     == "WESD", the maximum depth is multiplied by 5, then compared. Subsequent
#'     \emph{VALUE}s that remain unchanged after the first is removed are also
#'     removed.}
#'
#'     \item{}{\emph{"high_max"} - \emph{VALUE} should be converted to
#'     consistent units first (such as kPa). Removes all rows where the value
#'     exceeds the 3rd quartile plus \emph{iqr_cutoff} * the interquartile range
#'     of the yearly maximums for each station. Alternatively, yearly maximums
#'     can be found for clusters of stations by changing \emph{max_grouping}
#'     to the name of the cluster ID column. If there are
#'     \emph{max_yearly_outliers} or more values above the cutoff for a year
#'     and station, then the values are retained.}
#'
#'     \item{}{\emph{"corrupt_years"} - If there are too many observations
#'     removed by "county_max" or "state_max" for a year, then all
#'     observations from that year are removed for that station. Threshold set
#'     by \emph{yearly_threshold} parameter.}
#'   }
#' @param months The months to remove when "month" is a part of \emph{clean}.
#'   The default is 7 (Jul), 8 (Aug), and 9 (Sep).
#' @param max_grouping When "high_max" is part of \emph{clean}, this specifies
#'   what column to group by when finding interquartile ranges.
#' @param iqr_cutoff The number of interquartile ranges above the 3rd quartile
#'   to consider values outliers when "high_max" is part of \emph{clean}.
#' @param max_yearly_outliers If there are this many values for a year and
#'   station which are marked as maximum outliers when "high_max" is part of
#'   \emph{clean}, the values are considered as not outliers.
#' @param county_multiplier A multiplier for county max values to allow for
#'   more conservative "county_max" outlier removal by setting to greater than 1.
#' @param yearly_threshold The proportion (0 to 1) or number (integer) of
#'   observations that are removed by "county_max" or "state_max"
#'   to consider a year corrupted and remove it from a station record.
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
#'
#' @export
clean_station_data <- function(station_data, clean = "all", report = FALSE,
                               months = c(7, 8, 9), max_grouping = ID,
                               iqr_cutoff = 3, max_yearly_outliers = 5,
                               county_multiplier = 1, yearly_threshold = NULL) {
  `%>%` <- magrittr::`%>%`
  wesd_mult <- 5

  if ("high_max" %in% clean &&
       ("state_max" %in% clean || "county_max" %in% clean)) {
    stop("Cleaning 'high_max' and ('state_max' or 'county_max') have different",
         " requirements for values in the VALUE column, can't clean both.")
  }
  if ("corrupt_years" %in% clean &&
      (is.null(yearly_threshold) || yearly_threshold <= 0)) {
    stop("'corrupt_years' requires a valid yearly_threshold")
  }

  # Missing presumed zero flag removal
  #=============================================================================
  if ("mpzero" %in% clean || "all" %in% clean) {
    station_data <- station_data %>%
      dplyr::filter(MFLAG != "P" | is.na(MFLAG))
  }

  # Quality flag removal
  #=============================================================================
  if ("qflag" %in% clean || "all" %in% clean) {
    station_data <- station_data %>%
      dplyr::filter(QFLAG %in% c("", " ", NA))
  }

  # Month removal
  #=============================================================================
  if ("month" %in% clean || "all" %in% clean) {
    station_data <- station_data %>%
      dplyr::filter(!lubridate::month(DATE) %in% months)
  }

  # Corrupt years (part 1)
  #=============================================================================
  if ("corrupt_years" %in% clean) {
    year_numbers <- station_data %>%
      dplyr::mutate(MONTH = as.numeric(lubridate::month(DATE)),
                    YEAR = as.numeric(lubridate::year(DATE)),
                    YEAR = dplyr::if_else(MONTH > 9, YEAR + 1, YEAR)) %>%
      dplyr::group_by(ID, YEAR) %>%
      dplyr::summarise(COUNT = dplyr::n())
  }

  # State max
  #=============================================================================
  if ("state_max" %in% clean) {
    warning("Assuming VALUE is unaltered for cleaning via 'state_max'.")

    outliers <- station_data %>%
      dplyr::left_join(snowload2::ghcnd_stations %>%
                         dplyr::select(ID, STATE_MAX),
                       by = "ID") %>%
      dplyr::filter(!is.na(STATE_MAX),
                    ((ELEMENT == "SNWD" & VALUE > STATE_MAX) |
                     (ELEMENT == "WESD" & VALUE > STATE_MAX * wesd_mult)))

    station_data <- suppressMessages(
      dplyr::anti_join(station_data, outliers)
    )
  }

  # County max
  #=============================================================================
  if ("county_max" %in% clean) {
    warning("Assuming VALUE is unaltered for cleaning via 'county_max'.")

    outliers <- station_data %>%
      dplyr::mutate(MONTH = as.numeric(lubridate::month(DATE)),
                    YEAR = as.numeric(lubridate::year(DATE)),
                    YEAR = dplyr::if_else(MONTH > 9, YEAR + 1, YEAR)) %>%
      dplyr::arrange(ID, DATE) %>%
      dplyr::group_by(ID, ELEMENT, YEAR) %>%
      dplyr::mutate(DAYS = as.numeric(DATE - dplyr::lag(DATE)),
                    DAYS = dplyr::if_else(is.na(DAYS), 1, DAYS),
                    VDIFF = VALUE - dplyr::lag(VALUE),
                    VDIFF = dplyr::if_else(is.na(VDIFF), VALUE, VDIFF)) %>%
      dplyr::left_join(snowload2::ghcnd_stations %>%
                         dplyr::select(ID, STATE, COUNTY),
                       by = "ID") %>%
      dplyr::left_join(snowload2::county_maxes,
                       by = c("STATE", "COUNTY", "DAYS")) %>%
      dplyr::mutate(FLAG = !is.na(COUNTY_MAX) &
                      ((ELEMENT == "SNWD" &
                          VDIFF > COUNTY_MAX * county_multiplier) |
                       (ELEMENT == "WESD" &
                          VDIFF > COUNTY_MAX * wesd_mult * county_multiplier)),
                    FLAG = dplyr::if_else(VALUE == dplyr::lag(VALUE) &
                                            !is.na(VALUE == dplyr::lag(VALUE)),
                                          as.logical(NA), FLAG)) %>%
      tidyr::fill(FLAG) %>%
      dplyr::mutate(FLAG = if_else(is.na(FLAG), FALSE, FLAG)) %>%
      dplyr::filter(FLAG) %>%
      dplyr::ungroup()

    station_data <- suppressMessages(
      dplyr::anti_join(station_data, outliers)
    )
  }

  # Corrupt years (part 2)
  #=============================================================================
  if ("corrupt_years" %in% clean) {
    outliers <- station_data %>%
      dplyr::mutate(MONTH = as.numeric(lubridate::month(DATE)),
                    YEAR = as.numeric(lubridate::year(DATE)),
                    YEAR = dplyr::if_else(MONTH > 9, YEAR + 1, YEAR)) %>%
      dplyr::group_by(ID, YEAR) %>%
      dplyr::mutate(POST_COUNT = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(year_numbers, by = c("ID", "YEAR"))

    if (yearly_threshold < 1) {
      outliers <- outliers %>%
        dplyr::filter(POST_COUNT / COUNT <= (1 - yearly_threshold)) %>%
        dplyr::select(-YEAR, -MONTH, -COUNT, -POST_COUNT)
    } else {
      outliers <- outliers %>%
        dplyr::filter(COUNT - POST_COUNT >= yearly_threshold) %>%
        dplyr::select(-YEAR, -MONTH, -COUNT, -POST_COUNT)
    }

    station_data <- suppressMessages(
      dplyr::anti_join(station_data, outliers)
    )
  }

  # Max outlier removal
  #=============================================================================
  if ("high_max" %in% clean) {
    warning("Assuming VALUE have same units for cleaning via 'high_max'.")

    max_grouping <- dplyr::enquo(max_grouping)

    # year needed for finding maximums and number of yearly outliers
    outliers <- station_data %>%
      dplyr::mutate(MONTH = as.numeric(lubridate::month(DATE)),
                    YEAR = as.numeric(lubridate::year(DATE)),
                    YEAR = dplyr::if_else(MONTH > 9, YEAR + 1, YEAR))

    # find a cutoff for each station based on the stations interquartile range
    # of yearly maximums
    high_cutoff <- outliers %>%
      dplyr::group_by(!! max_grouping, YEAR) %>%
      dplyr::summarise(MAX = log(max(VALUE) + 1)) %>%
      dplyr::group_by(!! max_grouping) %>%
      dplyr::summarise(Q75 = stats::quantile(MAX, 0.75),
                       Q25 = stats::quantile(MAX, 0.25),
                       IQR = Q75 - Q25,
                       CUTOFF = dplyr::if_else(exp(Q75) - exp(Q25) < 1,
                                               max(MAX),
                                               Q75 + iqr_cutoff * IQR))

    # find which values are above the cutoff, keep the values if there are
    # enough for a given year
    outliers <- suppressMessages(dplyr::left_join(outliers, high_cutoff)) %>%
      dplyr::filter(log(VALUE + 1) > CUTOFF) %>%
      dplyr::group_by(!! max_grouping, YEAR) %>%
      dplyr::filter(dplyr::n() < max_yearly_outliers) %>%
      dplyr::ungroup()

    # remove the detected outliers
    station_data <- suppressMessages(
      dplyr::anti_join(station_data, outliers)
    )
  }

  return(station_data)
}
