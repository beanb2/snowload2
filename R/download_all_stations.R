#' Download all .dly weather data from the web.
#'
#' This function is designed to facilitate faster creation of data sets in
#' the \code{\link{get_station_data}} and \code{\link{get_state_data}}
#' functions. This is done by downloading all .dly files available, then
#' using the downloaded folder as the "source" instead of the web source.
#'
#' @param directory The directory to download the .dly files. Default is the
#' current working directory.
#'
#' @return Uncompressed ghcnd_all folder and ghcnd-version.txt in the supplied
#' directory. The source parameter for \code{\link{get_station_data}} and
#' \code{\link{get_state_data}} can be set to the ghcnd_all folder.
#'
#' @seealso
#'   \code{\link{get_station_data}} - get data by station.
#'
#' @export
download_all_stations <- function(directory = ".") {
  # Set file locations and names
  #=============================================================================
  if (directory == ".") directory <- getwd()
  directory <- path.expand(directory)
  # Remove trailing delimiter if present.
  directory <- gsub(directory, pattern = "/$", replacement = "")
  compressed_file <- paste0(directory, "/ghcnd_all.tar.gz")
  source <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd_all.tar.gz"

  # Download data
  #=============================================================================
  message("Downloading ghcnd_all.tar.gz to", directory, "...")
  utils::download.file(source, compressed_file)

  # Uncompress files and delete zip file
  #=============================================================================
  message("Uncompressing downloaded station data...")
  utils::untar(compressed_file, exdir = directory)
  file.remove(compressed_file)
  message("All files downloaded and uncompressed.")
}


