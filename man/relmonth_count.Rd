% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relmonth_count.R
\name{relmonth_count}
\alias{relmonth_count}
\title{Count the number of months intersecting a window where
  unique months are provided as a concantenated string.}
\usage{
relmonth_count(string, window, split = ":")
}
\arguments{
\item{string}{The string of months, with each month separated by a specfic token.}

\item{window}{The relevant window of months, given as a numeric vector.}

\item{split}{The month separator in each string.}
}
\value{
The number of months in the string that intersect the window.
}
\description{
Count the number of months intersecting a window where
  unique months are provided as a concantenated string.
}
\examples{
x <- "4:5:6"
relmonth_count(x, window = c(2, 4, 6))

}
