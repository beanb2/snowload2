#' GEV Distribution Value Estimation From Quantile
#'
#' Calculates the value obtained from the Generalized Extreme Value
#'   Distribution for a specific quantile. Vectorized for speed.
#'
#' @param p A quantile between 0 and 1, may be a vector.
#' @param loc The location parameter, may be a vector.
#' @param scale The scale parameter, may be a vector.
#' @param shape The shape parameter, may be a vector.
#'
#' @return A vector of values from the distribution based on the
#'   the provided quantiles.
#' @export
qgev <- function(p, loc = 0, scale = 1, shape = 0, tol = 1e-6){
  if(max(p) > 1 || min(p) < 0){
    warning("Quantiles must be between 0 and 1,
            replacing with tol and/or 1-tol")
  }

  # Restrict the quantiles (assuming that the user knows what they are doing).
  p <- pmax(pmin(1-tol, p), tol)

  # Set a switch function based on the value of the shape parameter.
  switch <- as.numeric(abs(shape) < 1e-8)

  # Swap out the switch function to avoid numerical imprecision issues.
  shape[abs(shape) < 1e-8] <- 1

  # Only one of the two expressions will ever be on at the same time.
  # This expression allows for fully vectorized operations.
  quant <- (1-switch)*(loc + scale*((-log(p))^(-shape) - 1)/shape) +
    (switch)*(loc - scale*log(-log(p)))

  return(quant)
}


#' GEV Distribution Quantile Estimation
#'
#' Calculates the value obtained from the Generalized Extreme Value
#'   Distribution for a specific quantile. Vectorized for speed.
#'
#' @param x A realization from the distribution.
#' @param loc The location parameter, may be a vector.
#' @param scale The scale parameter, may be a vector.
#' @param shape The shape parameter, may be a vector.
#'
#' @return A vector of quantiles.
#'
#' @export
pgev <- function(x, loc = 0, scale = 1, shape = 0){

  # Set a switch function based on the value of the shape parameter.
  switch <- as.numeric(abs(shape) < 1e-8)

  # Swap out the switch function to avoid numerical imprecision issues.
  shape[abs(shape) < 1e-8] <- 1

  s <- (x - loc) / scale

  # Only one of the two expressions will ever be on at the same time.
  # This expression allows for fully vectorized operations.
  quant <- (1-switch)*exp(-pmax(1+shape*s, 0)^(-1/shape)) + switch*exp(-exp(-s))

  return(quant)
}
