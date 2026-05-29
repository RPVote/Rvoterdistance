#' Calculate minimum distance in kilometers
#'
#' Given lat/lon vectors for voters and locations, returns the minimum
#' Haversine distance in kilometers for each voter to the nearest location.
#'
#' @param lat1 Numeric vector of voter latitudes.
#' @param lon1 Numeric vector of voter longitudes.
#' @param lat2 Numeric vector of location latitudes.
#' @param lon2 Numeric vector of location longitudes.
#'
#' @return Numeric vector of minimum distances in kilometers.
#' @export
#' @examples
#' data(meck_ev)
#' d <- dist_km(
#'   voter_meck$lat, voter_meck$long,
#'   early_meck$lat, early_meck$long
#' )
#' summary(d)
dist_km <- function(lat1, lon1, lat2, lon2) {
  .validate_coords(lat1, lon1, "voters")
  .validate_coords(lat2, lon2, "locations")
  raw <- cpp_k_nearest(lat1, lon1, lat2, lon2, 1L, FALSE)
  as.numeric(raw$distances) / 1000.0
}


#' Calculate minimum distance in miles
#'
#' Given lat/lon vectors for voters and locations, returns the minimum
#' Haversine distance in miles for each voter to the nearest location.
#'
#' @param lat1 Numeric vector of voter latitudes.
#' @param lon1 Numeric vector of voter longitudes.
#' @param lat2 Numeric vector of location latitudes.
#' @param lon2 Numeric vector of location longitudes.
#'
#' @return Numeric vector of minimum distances in miles.
#' @export
#' @examples
#' data(meck_ev)
#' d <- dist_mile(
#'   voter_meck$lat, voter_meck$long,
#'   early_meck$lat, early_meck$long
#' )
#' summary(d)
dist_mile <- function(lat1, lon1, lat2, lon2) {
  .validate_coords(lat1, lon1, "voters")
  .validate_coords(lat2, lon2, "locations")
  raw <- cpp_k_nearest(lat1, lon1, lat2, lon2, 1L, FALSE)
  as.numeric(raw$distances) / 1609.34
}


#' Haversine distance between two points
#'
#' Compute the Haversine (great-circle) distance between a single pair of
#' lat/lon coordinates.
#'
#' @param lat1 Latitude of point 1 (degrees).
#' @param lon1 Longitude of point 1 (degrees).
#' @param lat2 Latitude of point 2 (degrees).
#' @param lon2 Longitude of point 2 (degrees).
#' @param units One of `"meters"`, `"km"`, or `"miles"`. Default `"meters"`.
#'
#' @return Numeric scalar distance in the specified units.
#' @export
#' @examples
#' # New York to London
#' haversine(40.7128, -74.0060, 51.5074, -0.1278, units = "km")
haversine <- function(lat1, lon1, lat2, lon2,
                      units = c("meters", "km", "miles")) {
  units <- match.arg(units)
  d <- haversine_distance(lat1, lon1, lat2, lon2)
  switch(units,
    meters = d,
    km     = d / 1000.0,
    miles  = d / 1609.34
  )
}
