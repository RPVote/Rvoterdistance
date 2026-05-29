test_that("NA coordinates cause informative error", {
  expect_error(
    dist_km(c(47.6, NA), c(-122.3, -122.4), c(47.5), c(-122.2)),
    "NA values"
  )
})

test_that("mismatched lat/lon lengths cause error", {
  expect_error(
    dist_km(c(47.6, 47.7), c(-122.3), c(47.5), c(-122.2)),
    "same length"
  )
})

test_that("out-of-range latitudes cause error", {
  expect_error(
    dist_km(c(91), c(-122.3), c(47.5), c(-122.2)),
    "between -90 and 90"
  )
})

test_that("out-of-range longitudes cause error", {
  expect_error(
    dist_km(c(47.5), c(-181), c(47.5), c(-122.2)),
    "between -180 and 180"
  )
})

test_that("empty inputs cause error", {
  expect_error(
    dist_km(numeric(0), numeric(0), c(47.5), c(-122.2)),
    "must not be empty"
  )
})

test_that("missing coord_names for non-sf input causes error", {
  data(meck_ev, package = "Rvoterdistance")
  expect_error(
    nearest_location(voter_meck, early_meck),
    "coord names"
  )
})

test_that("k < 1 causes error", {
  data(meck_ev, package = "Rvoterdistance")
  expect_error(
    nearest_location(voter_meck, early_meck,
      voter_coords = c("lat", "long"),
      location_coords = c("lat", "long"),
      k = 0
    ),
    "k.*>= 1"
  )
})
