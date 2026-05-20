test_that("nearest_location with k=1 returns one row per voter", {
  data(meck_ev, package = "Rvoterdistance")
  result <- nearest_location(voter_meck, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"))
  expect_equal(nrow(result), nrow(voter_meck))
  expect_true("distance_km" %in% names(result))
  expect_true("distance_miles" %in% names(result))
  expect_true(all(result$distance_km > 0))
})

test_that("nearest_location with k=3 returns 3 rows per voter", {
  data(meck_ev, package = "Rvoterdistance")
  result <- nearest_location(voter_meck, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    k = 3, append_data = FALSE)
  expect_equal(nrow(result), nrow(voter_meck) * 3)
  expect_true(all(result$rank %in% 1:3))
})

test_that("k-nearest distances are non-decreasing per voter", {
  data(meck_ev, package = "Rvoterdistance")
  result <- nearest_location(voter_meck, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    k = 5, append_data = FALSE)
  # Check first 10 voters
  for (vid in unique(result$voter_id[1:10])) {
    sub <- result[result$voter_id == vid, ]
    expect_true(all(diff(sub$distance_m) >= 0))
  }
})

test_that("nearest_location with append_data=FALSE omits source columns", {
  data(meck_ev, package = "Rvoterdistance")
  result <- nearest_location(voter_meck, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    append_data = FALSE)
  # Should only have voter_id and distance columns
  expect_true("voter_id" %in% names(result))
  expect_false("lat" %in% names(result))
})

test_that("dist_km and dist_mile agree with nearest_location", {
  data(meck_ev, package = "Rvoterdistance")
  km_vec <- dist_km(voter_meck$lat, voter_meck$long,
                     early_meck$lat, early_meck$long)
  mi_vec <- dist_mile(voter_meck$lat, voter_meck$long,
                       early_meck$lat, early_meck$long)
  result <- nearest_location(voter_meck, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    append_data = FALSE)
  expect_equal(km_vec, result$distance_km, tolerance = 1e-6)
  expect_equal(mi_vec, result$distance_miles, tolerance = 1e-6)
})
