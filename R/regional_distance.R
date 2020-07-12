#' Get distances between data and regions.
#'
#' Finds distances in km between data provided as sf dataframe with point geometry
#' and regions provided as sf dataframe with polygon geometry.
#'
#'
#' @param data An sf data frame with longitude, latitude, and columns required
#' for modeling.
#' @param lon Name of longitude column (no quotes).
#' @param lat Name of latitude column (no quotes).
#' @param regions An sf dataframe with polygon geometry.
#' @param region_id Optional name of column in 'regions' that contains the id
#' that each region belongs to (no quotes). If null, it will be assumed that
#' each polygon is its own region (no regions have more than one polygon).
#' @param progress If true, a text progress bar is printed to the console.
#'
#' @return A matrix where each row corresponds one-to-one with each row in
#' provided 'data'. Matrix columns are either named with regions from 'region_id'
#' column of 'regions' or the row numbers of 'regions' if 'region_id' is NULL.
#' Values are in kilometers.
#'
#' @seealso
#'   \code{\link{regional_model}} - uses regional_distance for regional models.
#'
#'
#' @export
regional_distance <- function(data, lon, lat, regions, region_id = NULL,
                              progress = TRUE) {

  # Check input
  # ============================================================================
  if (!"data.frame" %in% class(data)) stop("data must be class 'data.frame'")
  if (!"sf" %in% class(regions)) stop("regions must be class 'sf'")

  if (!tryCatch(is.character(lon), error = function(e) FALSE)) {
    lon <- deparse(substitute(lon))
  }
  if (!tryCatch(is.character(lat), error = function(e) FALSE)) {
    lat <- deparse(substitute(lat))
  }

  sf_data <- sf::st_as_sf(data,
                          coords = c(lon, lat),
                          crs = 4326)

  # Create list of regions to check (if region_id is NULL, then each polygon
  #   is a separate region)
  if (missing(region_id)) {
    id_list <- 1:nrow(regions)
  } else {
    if (!tryCatch(is.character(region_id), error = function(e) FALSE)) {
      region_id <- deparse(substitute(region_id))
    }
    id_list <- sort(unique(as.character(regions[[region_id]])))
  }

  # Find distances between the data and each region
  # ============================================================================
  distances <- matrix(as.numeric(NA),
                      nrow = nrow(data),
                      ncol = length(id_list),
                      dimnames = list(NULL, id_list))

  # add progress bar
  if (progress) {
    pb <- txtProgressBar(min = 0, max = length(id_list), style = 3)
    i <- 1
  }

  for (id in id_list) {
    distances[, id] <- apply(
      sf::st_distance(sf_data, regions[regions[[region_id]] %in% id, ]),
      1,
      min
    )

    # update progress bar
    if (progress) {
      setTxtProgressBar(pb, i)
      i <- i + 1
    }
  }

  if (progress) cat("\n")

  return(distances / 1000)
}
