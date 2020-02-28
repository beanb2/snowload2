#' Fit distributions for snowload yearly maximums.
#'
#' This function will group by id variable(s) and return distribution parameters
#' for each group of values.
#'
#' @param station_data A data frame with yearly maximum station snow data (see
#' \code{\link{yearly_maximums}} to get yearly maximums).
#' @param id Column containing station ID's, may include other grouping
#'   variables.
#' @param value Column containing the observations to maximize
#' @param distr A distribution for fitting the data. Must be one of "lnorm" (log
#'   normal), "gamma", "gumbel", or "gev" (generalized extreme value).
#' @param method Method "mle" (maximum likelihood) will work for any
#' distribution. Method "lmoments" will only work with "gev" distribution.
#'
#' @return A data.frame where each row is a maximum for a given id and year:
#'   \describe{
#'     \item{}{\emph{id} - Given id column and possibly other grouping columns.}
#'     \item{}{\emph{DISTR} - Which distribution was fit.}
#'     \item{}{\emph{NFIT} - Number of values used for fitting. (The number
#'       of values used to fit the distribution is (1 - PROP_ZERO) * NFIT.)}
#'     \item{}{\emph{PAR1} - First fitting parameter. For "lnorm": meanlog,
#'       "gamma": shape, "gumbel": location, and "gev": location.}
#'     \item{}{\emph{PAR2} - Second fitting parameter. For "lnorm": sdlog,
#'       "gamma": rate, "gumbel": scale, and "gev": scale.}
#'     \item{}{\emph{PAR3} - Third fitting parameter. For "gev": shape.}
#'     \item{}{\emph{EVENT50} - Predicted value for 50 year event.}
#'   }
#'
#' @seealso
#'   \code{\link{yearly_maximums}} - get maximum values by "water year".
#'
#' @examples
#' x <- get_station_data(ghcnd_stations$ID[10000],
#'                       "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/")
#' xym <- yearly_maximums(x, ID, DATE, VALUE)
#' fit_dist(xym, ID, MAX, distr = "gev", method = "lmoments")
#'
#'
#' @export
fit_dist <- function(station_data, id, values, distr, method = "mle") {
  # Prepare column names and prepare data
  #=============================================================================
  id <- dplyr::enquo(id)
  values <- dplyr::enquo(values)

  station_data <- dplyr::group_by(station_data, !! id)


  # Choose functions for fit and q_function
  #=============================================================================
  if (distr == "lnorm") {
    # Log normal
    if (method == "mle") {
      fit <- function(x) {
        fitdistrplus::fitdist(x, distr = "lnorm", method = "mle",
                              keepdata = FALSE)$estimate
      }
    } else error("No method", method, "for distr", distr)

    q_function <- function(p, par1, par2) {
      stats::qlnorm(p = p, meanlog = par1, sdlog = par2)
    }

  } else if (distr == "gamma") {
    # Gamma
    if (method == "mle") {
      fit <- function(x) {
        fitdistrplus::fitdist(x, distr = "gamma", method = "mle",
                              keepdata = FALSE)$estimate
      }
    } else error("No method", method, "for distr", distr)

    q_function <- function(p, par1, par2) {
      stats::qgamma(p = p, shape = par1, rate = par2)
    }
  } else if (distr == "gumbel") {
    # Gumbel
    if (method == "mle") {
      fit <- function(x) {
        extRemes::fevd(x, type = "Gumbel", method = "MLE")$results$par
      }
    } else error("No method", method, "for distr", distr)

    q_function <- function(p, par1, par2) {
      mapply(extRemes::qevd, p = p, loc = par1, scale = par2,
             MoreArgs = list(type = "Gumbel"))
      #evd::qgev(p = p, loc = par1, scale = par2, shape = 0)
    }

  } else if (distr == "gev") {
    # Generalized extreme value
    if (method == "mle") {
      fit <- function(x) {
        extRemes::fevd(x, type = "GEV", method = "MLE")$results$par
      }
    } else if (method == "lmoments") {
      fit <- function(x) {
        extRemes::fevd(x, type = "GEV", method = "Lmoments")$results
      }
    } else error("No method", method, "for distr", distr)

    q_function <- function(p, par1, par2, par3) {
      mapply(extRemes::qevd, p = p, loc = par1, scale = par2, shape = par3,
             MoreArgs = list(type = "GEV"))
      #evd::qgev(p = p, loc = par1, scale = par2, shape = par3)
    }
  }else error("No distr", distr)



  # Wrapper functions for fit and q_function
  #=============================================================================
  fit_wrap <- function(x, f) {
    x <- x[x != 0 & !is.na(x)]
    if (length(x) <= 1) return(paste("NA", "NA", "NA"))
    dist <- try(f(x), silent = TRUE)
    if(inherits(dist, "try-error")){
      return(paste("NA", "NA", "NA"))
    }
    else return(paste(dist, collapse = " "))
  }

  event50 <- function(par1, par2, par3, zero, f) {
    out <- rep(as.numeric(NA), length(par1))
    keep <- !is.na(par1)
    if (sum(keep) == 0) {return(as.numeric(NA))}
    par1 <- par1[keep]
    par2 <- par2[keep]
    par3 <- par3[keep]
    zero <- zero[keep]

    p <- (.98 - zero)/(1-zero)
    if (all(is.na(par3))) {
      out[keep] <- f(p, par1, par2)
    } else {
      out[keep] <- f(p, par1, par2, par3)
    }
    return(out)
  }



  # Fit distributions
  #=============================================================================
  station_data <- dplyr::summarise(
    station_data,
    DISTR = distr,
    NFIT = sum(!is.na(!! values)),
    PROP_ZERO = sum(!! values == 0, na.rm = TRUE) / NFIT,
    PAR1 = fit_wrap(!! values, fit)
  )
  station_data <- tidyr::separate(
    station_data, col = PAR1,
    into = c("PAR1", "PAR2", "PAR3"),
    sep = " ", fill = "right", convert = TRUE
  )
  dplyr::mutate(station_data,
                EVENT50 = event50(PAR1, PAR2, PAR3, PROP_ZERO, q_function))
}


