#' Cluster stations that are near one another.
#'
#' Uses complete linkage clustering where each cluster has a maximum similarity
#' score of h between furthest stations in a cluster. dist_adj can be used to
#' scale distances between stations and elev_adj can be used to scale elevation
#' between stations. For example, distance can be scaled so that dist_adj = 4
#' miles is a one unit similarity between stations and elevation can be scaled
#' so that elev_adj = 50 is a one unit similarity between stations. Then two
#' stations that are separated by 4 miles and have a difference of 50 units of
#' elevation have a total similarity score of 2. h is used to specify a maximum
#' similarity score between stations in a cluster.
#'
#' @param lon A numeric vector of station longitudes.
#' @param lat A numeric vector of station latitudes.
#' @param elev A numeric vector of station elevations.
#' @param dist_adj Constant value in miles that scales the dissimilarity score
#' for geographic distance.
#' @param elev_adj Constant value that scales elevation dissimilarity.
#' @param h Maximum similarity score for the cluster analysis.
#'
#' @return A numeric vector of clusters, where each unique number represents a
#' cluster.
#'
#' @examples
#' # Simple example
#' subset <- snowload2::ghcnd_stations[10500:10600,]
#' cluster_stations(subset$LONGITUDE, subset$LATITUDE, subset$ELEVATION,
#'                  dist_adj = 4, elev_adj = 50, h = 2)
#'
#' Practical use example
#' library(dplyr)
#' cluster_test <- snowload2::ghcnd_stations %>%
#'   filter(STATE %in% c("UT", "NV")) %>%
#'   group_by(STATE) %>%
#'   mutate(CLUST = cluster_stations(LONGITUDE, LATITUDE, ELEVATION,
#'                                   dist_adj = 4,
#'                                   elev_adj = 50,
#'                                   h = 2)) %>%
#'   ungroup() %>%
#'   mutate(CLUST = sprintf(paste0(STATE, "%04d"), CLUST))
#'
#'
#' @export
cluster_stations <- function(lon, lat, elev, dist_adj, elev_adj, h) {
  # Find similarity matrix
  #=============================================================================
  # find distances between all locations in miles
  dist <- fields::rdist.earth(matrix(c(lon, lat), ncol = 2))

  # find difference in elevation between all locations
  delev <- abs(outer(elev, elev, "-"))

  # Produce "similarity matrix":
  # - distance of dist_adj km produces a score of 1
  # - elevation difference of elev_adj produces a score of 1
  similarity <- stats::as.dist((dist/dist_adj) + (delev/elev_adj))

  # Cluster and return cluster vector
  #=============================================================================
  # Cluster all stations with the farthest neighbors in a cluster
  clust <- stats::hclust(similarity, method = "complete")

  # Cut heirarchical clustering tree at h and return cluster id's
  stats::cutree(clust, h = h)
}
