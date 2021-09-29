#' Global Historical Climatology Network (GHCN-Daily) Stations
#'
#' Collected from
#' ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt.
#'
#' @format A data frame with 69163 rows and 12 variables:
#'
#'  \describe{
#'  \item{ID}{The station identification code.  Note that the first two
#'  characters denote the FIPS  country code, the third character
#'  is a network code that identifies the station numbering system
#'  used, and the remaining eight characters contain the actual
#'  station ID. \cr\cr
#'  See "ghcnd-countries.txt" for a complete list of country codes.\cr
#'  See "ghcnd-states.txt" for a list of state/province/territory codes.\cr\cr
#'  The network code  has the following five values:\cr\cr
#'  0 = unspecified (station identified by up to eight alphanumeric
#'  characters)\cr
#'  1 = Community Collaborative Rain, Hail,and Snow (CoCoRaHS)
#'  based identification number.\cr
#'  To ensure consistency with with GHCN Daily, all numbers in the original
#'  CoCoRaHS IDs have been left-filled to make them all four digits long.
#'  In addition, the characters "-" and "_" have been removed to ensure that
#'  the IDs do not exceed 11 characters when preceded by "US1". For example,
#'  the CoCoRaHS ID "AZ-MR-156" becomes "US1AZMR0156" in GHCN-Daily\cr
#'  C = U.S. Cooperative Network identification number (last six
#'  characters of the GHCN-Daily ID)\cr
#'  E = Identification number used in the ECA&D non-blended dataset
#'  M = World Meteorological Organization ID (last five characters of the
#'  GHCN-Daily ID)\cr
#'  N = Identification number used in data supplied by a National
#'  Meteorological or Hydrological Center\cr
#'  R = U.S. Interagency Remote Automatic Weather Station (RAWS) identifier\cr
#'  S = U.S. Natural Resources Conservation Service SNOwpack TELemtry
#'  (SNOTEL) station identifier\cr
#'  W = WBAN identification number (last five characters of the GHCN-Daily
#'  ID)}
#'  \item{LATITUDE}{The latitude of the station (in decimal degrees).}
#'  \item{LONGITUDE}{The longitude of the station (in decimal degrees).}
#'  \item{ELEVATION}{The elevation of the station (in meters).}
#'  \item{STATE}{The U.S. postal code for the state (for U.S. stations
#'  only).}
#'  \item{NAME}{The name of the station.}
#'  \item{GSN_FLAG}{A flag that indicates whether the station is part of
#'  the GCOS Surface Network (GSN). The flag is assigned by cross-referencing
#'  the number in the WMOID field with the official list of GSN stations.
#'  There are two possible values:\cr\cr
#'  Blank = non-GSN station or WMO Station number not available\cr
#'  GSN   = GSN station}
#'  \item{HCN_CRN_FLAG}{A flag that indicates whether the station is part
#'  of the U.S. CRN FLAG  Historical Climatology Network (HCN) or U.S.
#'  Climate Refererence Network (CRN).  There are three possible values:\cr\cr
#'  Blank = Not a member of the U.S. Historical Climatology or U.S. Climate
#'  Reference Networks\cr
#'  HCN   = U.S. Historical Climatology Network station\cr
#'  CRN   = U.S. Climate Reference Network or U.S. Regional Climate Network
#'  Station}
#'  \item{WMO_ID}{The World Meteorological Organization (WMO) number for the
#'  station. If the station has no WMO number (or one has not yet been
#'  matched to this station), then the field is blank.}
#'  \item{ECO3}{Identifier for the EPA level 3 ecoregion where the station
#'  is located. Obtained from
#'  ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/NA_CEC_Eco_Level3.zip}
#'  \item{STATE_MAX}{Maximum observed snow depth in mm for state. Obtained from
#'  https://www.ncdc.noaa.gov/extremes/scec/records.csv.}
#'  \item{COUNTY}{County where station is located.}
#'  }
"ghcnd_stations"


#' Removed values flagged by state and county max filters.
#'
#' Collected from state and county max data available at
#' https://www.ncdc.noaa.gov/snow-and-ice/snowfall-extremes/ and
#' https://www.ncdc.noaa.gov/extremes/scec/. All records are given
#' for snow depth and snowfall. WESD outliers are detected by assuming
#' a specific gravity of 0.5 (which is intentionally conservative).
#'
#' @format A data frame with 664,324 observations and 7 variables:
#'
#'  \describe{
#'  \item{ID}{The GHCND station identifier.}
#'  \item{DATE}{Day in yyyy-mm-dd format.}
#'  \item{ELEMENT}{Value indicating water equivalent of snow on the ground
#'    (WESD) or snow depth (SNWD).}
#'  \item{VALUE}{Either WESD in tenths of millimeters or SNWD in millimeters.}
#'  \item{FLAGGED}{0 - Value not flagged but manually detected during
#'  outlier search. 1 - Value flagged for exceeding state extreme snow depth.
#'  2- Value flagged for exceeding county extreme snowfall.}
#'  \item{OUTLIER}{Manual determination if the value was, in fact, and outlier.
#'  1 - yes, 0 - no, -1 - values removed for coverage issue (all zero values).}
#'  \item{TYPE}{Method in which outlier was detected. Options include:
#'  \itemize{
#'  \item state_flag: value was flagged for exceeding state snow depth or county snowfall records.
#'  \item heavy_tail: observations were investigated from a distribution with an unusually large, positive shape parameter.
#'  \item left_skew: observations were investigated from a distribution with an unusually large, negative shape parameter.
#'  \item region_shape: observations were investigated from a distribution with a shape parameter notably different than the
#'  shape parameter predicted via regional smoothing.
#'  \item yr50: observations were investigated because the resulting 50 year event was very different than the maximum max observation.
#'  \item state_scatter: observations were identified as having 50 year events significantly different from other observations in the same state.
#'  \item heavy_tail_2: observations with large, positive shape parameters as well as a maximum max 4 times larger than the estimated 50 year event.
#'  \item OR_WA_check: observations from a region-level investigation of observations in coastal Oregon and Washington.
#'  }}
#'  }
"outlier_combined"

#' Removed values flagged by state and county max filters - Scout Only Update.
#'
#' In an effort to employ a consistent outlier detection standard, Scout Jarman
#'   did a comprehensive check of all stations included in the outlier_combined
#'   daaset and reflagged values according to his standard.
#'
#' @format A data frame with 742,611 observations and 7 variables:
#'
#'  \describe{
#'  \item{ID}{The GHCND station identifier.}
#'  \item{DATE}{Day in yyyy-mm-dd format.}
#'  \item{ELEMENT}{Value indicating water equivalent of snow on the ground
#'    (WESD) or snow depth (SNWD).}
#'  \item{VALUE}{Either WESD in tenths of millimeters or SNWD in millimeters.}
#'  \item{FLAGGED}{0 - Value not flagged but manually detected during
#'  outlier search. 1 - Value flagged for exceeding state extreme snow depth.
#'  2- Value flagged for exceeding county extreme snowfall.}
#'  \item{OUTLIER}{Manual determination if the value was, in fact, and outlier.
#'  1 - yes, 0 - no, -1 - values removed for coverage issue (all zero values).}
#'  \item{TYPE}{Method in which outlier was detected. Options include:
#'  \itemize{
#'  \item state_flag: value was flagged for exceeding state snow depth or county snowfall records.
#'  \item heavy_tail: observations were investigated from a distribution with an unusually large, positive shape parameter.
#'  \item left_skew: observations were investigated from a distribution with an unusually large, negative shape parameter.
#'  \item region_shape: observations were investigated from a distribution with a shape parameter notably different than the
#'  shape parameter predicted via regional smoothing.
#'  \item yr50: observations were investigated because the resulting 50 year event was very different than the maximum max observation.
#'  \item state_scatter: observations were identified as having 50 year events significantly different from other observations in the same state.
#'  \item heavy_tail_2: observations with large, positive shape parameters as well as a maximum max 4 times larger than the estimated 50 year event.
#'  \item OR_WA_check: observations from a region-level investigation of observations in coastal Oregon and Washington.
#'  }}
#'  }
"outlier_combined_alt"


#' Max 1,2,...,10 day snowfall totals for each county.
#'
#' Collected from
#' https://www.ncdc.noaa.gov/snow-and-ice/snowfall-extremes/. 1, 2, and 3 day
#' data are from this source. 4,5,...10 day data are calculated by day
#' * (3day_value / 3).
#'
#' @format A data frame with 31430 rows and 4 variables:
#'
#'  \describe{
#'  \item{STATE}{The U.S. postal code for the state (for U.S. stations
#'  only).}
#'  \item{COUNTY}{The name of the county.}
#'  \item{DAYS}{Number of days over which the maximum accumulated.}
#'  \item{COUNTY_MAX}{Maximum snow in mm of snowfal accumulated over the number
#'  of given DAYS.}
#'  }
"county_maxes"



#' Alternate outliers as verified through secondary manual checks.
#'
#'
#'  \describe{
#'  \item{ID}{The GHCND station identifier.}
#'  \item{DATE}{Day in yyyy-mm-dd format.}
#'  \item{ELEMENT}{Value indicating water equivalent of snow on the ground
#'    (WESD) or snow depth (SNWD).}
#'  \item{VALUE}{Either WESD in tenths of millimeters or SNWD in millimeters.}
#'  \item{OUTLIER}{Manual determination if the value was, in fact, and outlier.
#'  1 - yes, 0 - no, -1 - values removed for coverage issue (all zero values).}
#'  }
"outlier_combined_alt"



