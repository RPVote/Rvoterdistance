test_that("nearest_location accepts sf POINT objects", {
  skip_if_not_installed("sf")
  data(meck_ev, package = "Rvoterdistance")

  voters_sf <- sf::st_as_sf(voter_meck, coords = c("long", "lat"), crs = 4326)
  locs_sf   <- sf::st_as_sf(early_meck, coords = c("long", "lat"), crs = 4326)

  result_sf <- nearest_location(voters_sf, locs_sf, append_data = FALSE)
  result_df <- nearest_location(voter_meck, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    append_data = FALSE)

  expect_equal(result_sf$distance_m, result_df$distance_m, tolerance = 1e-6)
})

test_that("sf objects with non-WGS84 CRS are transformed with message", {
  skip_if_not_installed("sf")
  data(meck_ev, package = "Rvoterdistance")

  voters_sf <- sf::st_as_sf(voter_meck, coords = c("long", "lat"), crs = 4326)
  # Transform to UTM zone 17N
  voters_utm <- sf::st_transform(voters_sf, 32617)
  locs_sf <- sf::st_as_sf(early_meck, coords = c("long", "lat"), crs = 4326)

  expect_message(
    result <- nearest_location(voters_utm, locs_sf, append_data = FALSE),
    "Transforming"
  )
  expect_equal(nrow(result), nrow(voter_meck))
})

test_that("non-POINT sf geometries are rejected", {
  skip_if_not_installed("sf")
  line <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2)),
    crs = 4326
  )
  line_sf <- sf::st_sf(data.frame(id = 1), geometry = line)
  pt_sf <- sf::st_as_sf(data.frame(lon = 0, lat = 0),
                          coords = c("lon", "lat"), crs = 4326)

  expect_error(
    nearest_location(line_sf, pt_sf),
    "POINT geometries"
  )
})
