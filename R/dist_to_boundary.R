#' Distance from voters to a geographic boundary
#'
#' Computes the minimum great-circle distance from each voter to the
#' nearest point on a boundary line or polygon edge. The boundary can
#' represent a river, state border, or any other geographic feature
#' provided as an `sf` geometry object.
#'
#' For polygon inputs the distance is measured to the polygon's
#' **boundary** (perimeter), not to its interior. A point inside the
#' polygon returns the positive distance to the nearest edge.
#'
#' Core computation uses the spherical cross-track distance formula
#' implemented in C++ for performance, with bounding-box pruning to
#' skip distant segments.
#'
#' @param voters A data frame, matrix, or `sf` POINT object containing
#'   voter locations. See [nearest_location()] for details on input
#'   formats.
#' @param boundary An `sf` or `sfc` object with LINESTRING,
#'   MULTILINESTRING, POLYGON, or MULTIPOLYGON geometry. Will be
#'   transformed to WGS-84 (EPSG:4326) if needed.
#' @param voter_coords Character vector of length 2 giving the column
#'   names for latitude and longitude in `voters` (e.g.,
#'   `c("lat", "lon")`). Required if `voters` is a data frame; ignored
#'   for `sf` objects.
#' @param units One of `"km"` (default), `"miles"`, or `"meters"`.
#' @param progress Logical; show progress messages? Default `TRUE`.
#'
#' @return Numeric vector of distances (one per voter) in the requested
#'   units.
#'
#' @export
#' @examples
#' \dontrun{
#' library(sf)
#' # Create a simple north-south boundary line
#' border <- st_sf(
#'   geometry = st_sfc(
#'     st_linestring(matrix(c(-109.05, 31.33, -109.05, 37.0),
#'       ncol = 2, byrow = TRUE
#'     )),
#'     crs = 4326
#'   )
#' )
#'
#' voters <- data.frame(lat = c(35.08, 32.0), lon = c(-106.65, -108.5))
#' dist_to_boundary(voters, border, voter_coords = c("lat", "lon"))
#' }
dist_to_boundary <- function(voters, boundary,
                             voter_coords = NULL,
                             units = c("km", "miles", "meters"),
                             progress = TRUE) {
  units <- match.arg(units)

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Package 'sf' is required for dist_to_boundary(). ",
      "Install it with install.packages('sf').",
      call. = FALSE
    )
  }

  # ── Extract voter coordinates ─────────────────────────────────────
  voter_mat <- .extract_coords(voters, voter_coords, "voters")
  vlat <- voter_mat[, 1]
  vlon <- voter_mat[, 2]
  .validate_coords(vlat, vlon, "voters")

  # ── Validate and prepare boundary ─────────────────────────────────
  # Accept sf or sfc

  if (inherits(boundary, "sf")) {
    geom <- sf::st_geometry(boundary)
  } else if (inherits(boundary, "sfc")) {
    geom <- boundary
  } else {
    stop(
      "'boundary' must be an sf or sfc object with line or polygon geometry.",
      call. = FALSE
    )
  }

  # Check geometry type
  geom_type <- sf::st_geometry_type(geom, by_geometry = FALSE)
  valid_types <- c(
    "LINESTRING", "MULTILINESTRING",
    "POLYGON", "MULTIPOLYGON",
    "GEOMETRY"
  )
  if (!as.character(geom_type) %in% valid_types) {
    stop(
      "'boundary' geometry type must be LINESTRING, MULTILINESTRING, ",
      "POLYGON, or MULTIPOLYGON. Got: ", geom_type,
      call. = FALSE
    )
  }

  # Transform to WGS-84 if needed
  crs <- sf::st_crs(geom)
  if (!is.na(crs) && !is.na(crs$epsg) && crs$epsg != 4326L) {
    message(
      "Transforming boundary from EPSG:", crs$epsg,
      " to WGS-84 (EPSG:4326)"
    )
    geom <- sf::st_transform(geom, 4326L)
  }

  # Convert polygons to linestrings (boundary edges)
  per_geom_type <- as.character(sf::st_geometry_type(geom))
  has_polygon <- any(per_geom_type %in% c("POLYGON", "MULTIPOLYGON"))
  if (has_polygon) {
    geom <- sf::st_cast(geom, "MULTILINESTRING")
  }

  # ── Extract segment start/end coordinates ─────────────────────────
  segments <- .extract_segments(geom)

  if (nrow(segments) == 0L) {
    stop("No valid segments found in 'boundary' geometry.", call. = FALSE)
  }

  if (progress) {
    message(
      "  Boundary: ", nrow(segments), " segments | ",
      "Voters: ", length(vlat)
    )
  }

  # ── Call C++ ──────────────────────────────────────────────────────
  dist_m <- cpp_dist_to_boundary(
    vlat, vlon,
    segments$start_lat, segments$start_lon,
    segments$end_lat, segments$end_lon,
    progress
  )

  # ── Convert units ────────────────────────────────────────────────
  switch(units,
    km     = dist_m / 1000.0,
    miles  = dist_m / 1609.34,
    meters = dist_m
  )
}


#' Extract line segments from an sfc geometry
#'
#' Takes an sfc object (LINESTRING or MULTILINESTRING) and returns a
#' data.frame of segment start/end coordinates.
#' @param geom An sfc object.
#' @return data.frame with columns: start_lat, start_lon, end_lat, end_lon
#' @noRd
.extract_segments <- function(geom) {
  coords <- sf::st_coordinates(geom)

  # Determine the grouping column (L1 for LINESTRING, L2 for MULTI, etc.)
  # st_coordinates returns X, Y, and L1, L2, ... columns
  coord_cols <- colnames(coords)
  group_cols <- coord_cols[grepl("^L[0-9]+$", coord_cols)]

  if (length(group_cols) == 0L) {
    # Single geometry, no grouping — treat all as one line
    n <- nrow(coords)
    if (n < 2L) {
      return(data.frame(
        start_lat = numeric(0), start_lon = numeric(0),
        end_lat = numeric(0), end_lon = numeric(0)
      ))
    }
    return(data.frame(
      start_lat = coords[-n, "Y"],
      start_lon = coords[-n, "X"],
      end_lat   = coords[-1, "Y"],
      end_lon   = coords[-1, "X"]
    ))
  }

  # Create a composite group ID from all L columns
  # Segments only connect consecutive points within the same group
  if (length(group_cols) == 1L) {
    group_id <- coords[, group_cols[1]]
  } else {
    # Paste all L columns to create unique group identifiers
    group_id <- apply(coords[, group_cols, drop = FALSE], 1, paste, collapse = "-")
  }

  # Identify valid segment pairs: consecutive rows in the same group
  n <- nrow(coords)
  same_group <- group_id[-n] == group_id[-1]

  data.frame(
    start_lat = coords[-n, "Y"][same_group],
    start_lon = coords[-n, "X"][same_group],
    end_lat   = coords[-1, "Y"][same_group],
    end_lon   = coords[-1, "X"][same_group]
  )
}
