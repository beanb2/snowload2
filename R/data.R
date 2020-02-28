#' Global Historical Climatology Network (GHCN-Daily) Stations
#'
#' Collected from
#' ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt.
#'
#' @format A data frame with 115081 rows and 9 variables:
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
#'  is located.}
#'  }
"ghcnd_stations"

