#' Extract lat/lon coordinates from various input types
#' @param data A data.frame, matrix, or sf POINT object.
#' @param coord_names Character vector of length 2: c("lat_col", "lon_col").
#' @param label Label for error messages (e.g., "voters" or "locations").
#' @return A two-column numeric matrix with columns lat and lon.
#' @noRd
.extract_coords <- function(data, coord_names, label) {

  # Case 1: sf object
  if (inherits(data, "sf")) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' is required to process sf objects. ",
           "Install it with install.packages('sf').",
           call. = FALSE)
    }
    geom <- sf::st_geometry(data)
    if (!inherits(geom, "sfc_POINT")) {
      stop("'", label, "' must contain POINT geometries, not ",
           class(geom)[1], call. = FALSE)
    }
    # Ensure WGS-84 (EPSG:4326)
    crs <- sf::st_crs(data)
    if (!is.na(crs) && !is.na(crs$epsg) && crs$epsg != 4326L) {
      message("Transforming ", label, " from EPSG:", crs$epsg,
              " to WGS-84 (EPSG:4326)")
      data <- sf::st_transform(data, 4326L)
      geom <- sf::st_geometry(data)
    }
    coords_mat <- sf::st_coordinates(geom)
    # st_coordinates returns X (lon), Y (lat) -- we need lat, lon
    return(cbind(lat = coords_mat[, "Y"], lon = coords_mat[, "X"]))
  }

  # Case 2: data.frame or matrix with named columns
  if (is.null(coord_names) || length(coord_names) != 2L) {
    stop("'", label, "' is not an sf object, so you must supply ",
         "coord names as c('lat_col', 'lon_col')", call. = FALSE)
  }

  if (is.data.frame(data)) {
    missing_cols <- setdiff(coord_names, names(data))
    if (length(missing_cols) > 0) {
      stop("Column(s) not found in ", label, ": ",
           paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    return(as.matrix(data[, coord_names, drop = FALSE]))
  }

  if (is.matrix(data)) {
    if (ncol(data) < 2) {
      stop("'", label, "' matrix must have at least 2 columns", call. = FALSE)
    }
    return(data[, 1:2, drop = FALSE])
  }

  stop("'", label, "' must be a data.frame, matrix, or sf object", call. = FALSE)
}


#' Validate coordinate vectors
#' @param lat Numeric vector of latitudes.
#' @param lon Numeric vector of longitudes.
#' @param label Label for error messages.
#' @noRd
.validate_coords <- function(lat, lon, label) {
  if (length(lat) == 0L || length(lon) == 0L) {
    stop("'", label, "' coordinates must not be empty", call. = FALSE)
  }
  if (length(lat) != length(lon)) {
    stop("'", label, "' lat and lon vectors must be the same length",
         call. = FALSE)
  }
  if (any(is.na(lat)) || any(is.na(lon))) {
    stop("NA values found in '", label,
         "' coordinates. Remove NAs before calling this function.",
         call. = FALSE)
  }
  if (any(lat < -90 | lat > 90)) {
    stop("'", label, "' latitudes must be between -90 and 90", call. = FALSE)
  }
  if (any(lon < -180 | lon > 180)) {
    stop("'", label, "' longitudes must be between -180 and 180",
         call. = FALSE)
  }
}


#' Convert distance to meters
#' @noRd
.to_meters <- function(dist, units) {
  switch(units,
    meters = dist,
    km     = dist * 1000.0,
    miles  = dist * 1609.34
  )
}


#' Format k-nearest C++ output into a data frame
#' @noRd
.format_knearest_result <- function(raw, k, voters, locations, units,
                                    append_data) {
  n_voters <- nrow(raw$distances)
  # k may have been clamped by C++ to min(k, n_locations)
  k <- ncol(raw$distances)

  if (k == 1L) {
    # Simple case: one row per voter
    dist_m <- as.numeric(raw$distances)
    loc_idx <- as.integer(raw$indices)

    out <- data.frame(
      voter_id       = seq_len(n_voters),
      distance_m     = dist_m,
      distance_km    = dist_m / 1000.0,
      distance_miles = dist_m / 1609.34,
      stringsAsFactors = FALSE
    )

    if (append_data) {
      voter_df <- .as_plain_df(voters)
      loc_df   <- .as_plain_df(locations)
      out <- cbind(voter_df, loc_df[loc_idx, , drop = FALSE],
                   out[, -1, drop = FALSE])
      rownames(out) <- NULL
    }

    return(out)
  }

  # k > 1: long format, one row per voter-location pair
  voter_ids <- rep(seq_len(n_voters), each = k)
  ranks     <- rep(seq_len(k), times = n_voters)
  dist_m    <- as.numeric(t(raw$distances))
  loc_idx   <- as.integer(t(raw$indices))

  out <- data.frame(
    voter_id       = voter_ids,
    rank           = ranks,
    location_id    = loc_idx,
    distance_m     = dist_m,
    distance_km    = dist_m / 1000.0,
    distance_miles = dist_m / 1609.34,
    stringsAsFactors = FALSE
  )

  if (append_data) {
    voter_df <- .as_plain_df(voters)
    loc_df   <- .as_plain_df(locations)
    voter_expanded <- voter_df[voter_ids, , drop = FALSE]
    loc_expanded   <- loc_df[loc_idx, , drop = FALSE]
    out <- cbind(voter_expanded, loc_expanded,
                 out[, c("rank", "distance_m", "distance_km",
                         "distance_miles")])
    rownames(out) <- NULL
  }

  out
}


#' Format threshold C++ output into a data frame
#' @noRd
.format_threshold_result <- function(raw, voters, locations, units,
                                     append_data) {
  n_voters <- length(raw)
  pieces <- vector("list", n_voters)

  for (i in seq_len(n_voters)) {
    entry <- raw[[i]]
    dists <- entry$distances
    idxs  <- entry$indices

    if (length(dists) == 0L) {
      pieces[[i]] <- data.frame(
        voter_id = i, rank = NA_integer_, location_id = NA_integer_,
        distance_m = NA_real_, distance_km = NA_real_,
        distance_miles = NA_real_,
        stringsAsFactors = FALSE
      )
    } else {
      ord <- order(dists)
      pieces[[i]] <- data.frame(
        voter_id       = i,
        rank           = seq_along(dists),
        location_id    = idxs[ord],
        distance_m     = dists[ord],
        distance_km    = dists[ord] / 1000.0,
        distance_miles = dists[ord] / 1609.34,
        stringsAsFactors = FALSE
      )
    }
  }

  out <- do.call(rbind, pieces)

  if (append_data) {
    voter_df <- .as_plain_df(voters)
    loc_df   <- .as_plain_df(locations)
    voter_expanded <- voter_df[out$voter_id, , drop = FALSE]
    non_na_locs <- !is.na(out$location_id)
    loc_expanded <- loc_df[1, , drop = FALSE][rep(NA_integer_, nrow(out)), ,
                                               drop = FALSE]
    loc_expanded[non_na_locs, ] <- loc_df[out$location_id[non_na_locs], ,
                                           drop = FALSE]
    out <- cbind(voter_expanded, loc_expanded,
                 out[, c("rank", "distance_m", "distance_km",
                         "distance_miles")])
    rownames(out) <- NULL
  }

  out
}


#' Convert sf or tibble to plain data.frame, dropping geometry
#' @noRd
.as_plain_df <- function(x) {
  if (inherits(x, "sf")) {
    x <- as.data.frame(x)
    x$geometry <- NULL
  }
  as.data.frame(x)
}
