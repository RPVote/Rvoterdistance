#' Find nearest polling locations for each voter
#'
#' Calculates the distance between each voter and a set of polling/drop box
#' locations using the Haversine formula. Can return the single nearest
#' location, the k nearest, or all locations within a distance threshold.
#'
#' @param voters A data frame, matrix, or `sf` POINT object containing voter
#'   locations. If a data frame or matrix, must contain lat/lon columns
#'   specified by `voter_coords`.
#' @param locations A data frame, matrix, or `sf` POINT object containing
#'   polling/drop box locations. If a data frame or matrix, must contain
#'   lat/lon columns specified by `location_coords`.
#' @param voter_coords Character vector of length 2: `c("lat_col", "lon_col")`
#'   identifying the latitude and longitude columns in `voters`. Ignored if
#'   `voters` is an `sf` object.
#' @param location_coords Character vector of length 2:
#'   `c("lat_col", "lon_col")` identifying the latitude and longitude columns
#'   in `locations`. Ignored if `locations` is an `sf` object.
#' @param k Integer. Number of nearest locations to return per voter.
#'   Default `1`.
#' @param max_dist Numeric or `NULL`. If not `NULL`, return all locations
#'   within this distance of each voter. Units controlled by `units`.
#'   Overrides `k`.
#' @param units Character. One of `"km"`, `"miles"`, or `"meters"`.
#'   Default `"km"`.
#' @param append_data Logical. If `TRUE` (default), include voter and matched
#'   location columns in the output. When `k = 1`, also appends the matched
#'   location row.
#' @param progress Logical. If `TRUE`, print progress for large computations.
#'   Default `FALSE`.
#'
#' @return A data frame. If `k = 1` and `max_dist` is `NULL`: one row per
#'   voter with distance columns (`distance_m`, `distance_km`,
#'   `distance_miles`). If `k > 1` or `max_dist` is not `NULL`: one row per
#'   voter-location pair with a `rank` column.
#'
#' @export
#' @examples
#' data(meck_ev)
#'
#' # Nearest single location for each voter
#' result <- nearest_location(voter_meck, early_meck,
#'   voter_coords = c("lat", "long"),
#'   location_coords = c("lat", "long")
#' )
#' head(result)
#'
#' # 3 nearest locations per voter
#' result_k3 <- nearest_location(voter_meck, early_meck,
#'   voter_coords = c("lat", "long"),
#'   location_coords = c("lat", "long"),
#'   k = 3
#' )
#' head(result_k3)
#'
#' # All locations within 10 km
#' result_10km <- nearest_location(voter_meck, early_meck,
#'   voter_coords = c("lat", "long"),
#'   location_coords = c("lat", "long"),
#'   max_dist = 10, units = "km"
#' )
#' head(result_10km)
nearest_location <- function(voters,
                             locations,
                             voter_coords = NULL,
                             location_coords = NULL,
                             k = 1L,
                             max_dist = NULL,
                             units = c("km", "miles", "meters"),
                             append_data = TRUE,
                             progress = FALSE) {
  units <- match.arg(units)

  # --- Extract coordinates ---
  v_coords <- .extract_coords(voters, voter_coords, "voters")
  l_coords <- .extract_coords(locations, location_coords, "locations")

  voter_lat <- v_coords[, 1]
  voter_lon <- v_coords[, 2]
  loc_lat <- l_coords[, 1]
  loc_lon <- l_coords[, 2]

  # --- Input validation ---
  .validate_coords(voter_lat, voter_lon, "voters")
  .validate_coords(loc_lat, loc_lon, "locations")

  k <- as.integer(k)
  if (k < 1L) stop("'k' must be >= 1", call. = FALSE)

  # --- Dispatch to C++ ---
  if (!is.null(max_dist)) {
    # Threshold mode
    max_dist_m <- .to_meters(max_dist, units)
    raw <- cpp_within_threshold(
      voter_lat, voter_lon, loc_lat, loc_lon,
      max_dist_m, progress
    )
    result <- .format_threshold_result(
      raw, voters, locations, units,
      append_data
    )
  } else {
    # k-nearest mode
    raw <- cpp_k_nearest(voter_lat, voter_lon, loc_lat, loc_lon, k, progress)
    result <- .format_knearest_result(
      raw, k, voters, locations, units,
      append_data
    )
  }

  result
}
