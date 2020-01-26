#' Organize .dly weather data downloaded from the web.
#'
#' These functions are designed to take NOAA weather data organized in .dly
#' format, extract snow depth, WESD, or other climate information, and return
#' a neatly organized data frame.
#'
#' @describeIn get_station_data Uses station identification codes to gather
#'   data from the proper .dly files.
#'
#' @param stations A vector of station identification codes. See the internal
#'   data set ghcnd_stations for station id's and descriptions.
#' @param source If downloading from the web, source should be
#'   "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/". For loading many
#'   stations/states it is faster to download
#'   ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd_all.tar.gz (~3GB),
#'   extract all files (~30GB), then use the file path to the folder with
#'   all extracted .dly files as the source.
#' @param elem The climate variables to extract. Default is snow depth in mm
#'   ("SNWD") and water depth equivalent of snowfall in mm ("WESD"). For other
#'   climate variables see section three of
#'   ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt.
#' @param progress If true, a text progress bar is printed to the console.
#'   Only relevant when more than one station is requested.
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
#' @examples
#' get_station_data(ghcnd_stations$ID[10000],
#'                  "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/")
#'
#'
#' @export
get_station_data <- function(stations, source, elem = c("SNWD", "WESD"), progress = TRUE){
  # Set constants
  #=============================================================================
  files <- paste0(source, stations, ".dly")
  month_days <- 1:31
  max_days <- max(month_days)

  # Indices for fixed width file item locations identified from:
  # ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
  # Section 3
  id_start <- 1
  id_end <- 11
  year_start <- 12
  year_end <- 15
  month_start <- 16
  month_end <- 17
  elem_start <- 18
  elem_end <- 21
  value_start <- month_days * 8 + 14
  value_end <- value_start + 4
  mflag_start <- value_end + 1
  mflag_end <- mflag_start
  qflag_start <- mflag_start + 1
  qflag_end <- qflag_start
  sflag_start <- qflag_start + 1
  sflag_end <- sflag_start


  # Define helper functions
  #=============================================================================
  # extract()
  #
  # A wrapper for vapply with substring that extracts a vector of characters
  # from each string
  extract <- function(strings, start, end) {
    vapply(strings,
           substring,
           character(length(start)),
           USE.NAMES = FALSE,
           start,
           end)
  }

  # extract_dates()
  #
  # A wrapper for extract that extracts a vector of dates for a given month in
  # each string
  extract_dates <- function(strings) {
    paste(rep(extract(strings, year_start, year_end), each = max_days),
          rep(extract(strings, month_start, month_end), each = max_days),
          rep(month_days, times = length(strings)),
          sep = "-")
  }


  # Extract data for each station in station vector
  #=============================================================================
  data_list <- vector("list", length(files))
  # Create a progress bar for the data download if requested. (utils package)
  if(progress){
  pb <- txtProgressBar(min = 0, max = length(files), style = 3)
  }
  for (i in 1:length(files)) {
    # Read the lines from the FTP file
    dly <- try(readLines(files[i]), silent = TRUE)
    if(inherits(dly, "try-error")){
      print(paste("No viable file found at ", files[i], sep = ""))
      data_list[[i]] <- NULL
      next
    }

    # Remove lines that do not represent requried elements
    dly <- dly[substring(dly, elem_start, elem_end) %in% elem]


    # Manipulate lines into data.frame
    data_list[[i]] <- data.frame(
      "ID" = rep(substring(dly, id_start, id_end),
                 each = max_days),
      "DATE" = as.Date(extract_dates(dly)),
      "ELEMENT" = rep(substring(dly, elem_start, elem_end),
                      each = max_days),
      "VALUE" = as.numeric(extract(dly, value_start, value_end)),
      "MFLAG" = as.character(extract(dly, mflag_start, mflag_end)),
      "QFLAG" = as.character(extract(dly, qflag_start, qflag_end)),
      "SFLAG" = as.character(extract(dly, sflag_start, sflag_end)),
      stringsAsFactors = FALSE
    )

    if (nrow(data_list[[i]]) == 0) {
      data_list[[i]] <- NULL
    }

    if(progress){
      setTxtProgressBar(pb, i)
    }
  }

  # Combine into single data.frame and remove missing values
  #=============================================================================
  tdata <- dplyr::bind_rows(data_list)

  # Return data.frame
  if (is.null(tdata) || nrow(tdata) == 0) {
    return(NULL)
  } else {
    tdata <- tdata[tdata$VALUE != -9999,] # -9999 is defined as missing
    return(tdata)
  }

}


#' @describeIn get_station_data Wrapper for get_station_data that extracts all
#'   station data for a given state(s) by passing all stations from internal
#'   data source ghcnd_stations with corresponding state(s).
#' @param states A vector of states, see the internal data set ghcnd_stations
#'   for valid states and associated stations.
#' @export
get_state_data <- function(states, source, elem = c("SNWD", "WESD"), progress = TRUE) {
  ghcnd_stations <- snowload2::ghcnd_stations
  stations <- ghcnd_stations$ID[ghcnd_stations$STATE %in% states]
  get_station_data(stations, source, elem, progress)
}









