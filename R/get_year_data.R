#' Organize .csv weather data downloaded from the web.
#'
#' Takes yearly NOAA weather data organized in .csv format, extract snow depth,
#' WESD, or other climate information, and return a neatly organized data frame.
#'
#' @param year A vector of years where data is to be gathered.
#' @param source If downloading from the web, source should be
#'   "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/by_year". Data can be
#'   manually downloaded, then use the file path to the folder with
#'   extracted .csv files as the source.
#' @param elem The climate variables to extract. Default is snow depth in mm
#'   ("SNWD") and water depth equivalent of snowfall in mm ("WESD"). For other
#'   climate variables see section three of
#'   ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt.
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
#'   \code{\link{get_station_data}} - get data by station.
#'
#'   \code{\link{get_state_data}} - get data by state.
#'
#' @examples
#' get_year_data(1770,
#'               "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/",
#'               elem = c("TMAX", "TMIN"))
#'
#' @export
get_year_data <- function(year, source, elem = c("SNWD", "WESD")){
  # Set constants
  #=============================================================================
  names <- c("ID", "DATE", "ELEMENT", "VALUE",
             "MFLAG", "QFLAG", "SFLAG", "TIME")
  files <- paste0(source, year, ".csv.gz")
  columns <- readr::cols(ID = readr::col_character(),
                         DATE = readr::col_date(format = "%Y%m%d"),
                         ELEMENT = readr::col_character(),
                         VALUE = readr::col_double(),
                         MFLAG = readr::col_character(),
                         QFLAG = readr::col_character(),
                         SFLAG = readr::col_character(),
                         TIME = readr::col_character())

  # Extract data for each year in year vector
  #=============================================================================
  data_list <- vector("list", length(files))

  for (i in 1:length(files)) {
    df <- try(readr::read_csv(files[i], col_names = names, col_types = columns),
              silent = TRUE)
    if(inherits(df, "try-error")){
      print(paste("No viable file found at ", files[i], sep = ""))
      data_list[[i]] <- NULL
      next
    }

    # select correct elements and remove TIME
    df <- df[df$ELEMENT %in% elem,]
    data_list[[i]] <- df[,1:7]
  }

  # Combine into single data.frame and remove missing values
  #=============================================================================
  tdata <- dplyr::bind_rows(data_list)
  tdata <- tdata[tdata$VALUE != -9999,]

  # Return data.frame
  if (nrow(tdata) == 0) {
    return(NULL)
  } else {
    return(tdata)
  }
}


