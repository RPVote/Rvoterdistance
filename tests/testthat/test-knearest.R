test_that("k is clamped to number of locations", {
  data(meck_ev, package = "Rvoterdistance")
  n_locs <- nrow(early_meck)
  result <- nearest_location(voter_meck[1:5, ], early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    k = n_locs + 10, append_data = FALSE
  )
  expect_equal(nrow(result), 5 * n_locs)
})

test_that("threshold returns only nearby locations", {
  data(meck_ev, package = "Rvoterdistance")
  result <- nearest_location(voter_meck[1:20, ], early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    max_dist = 5, units = "km",
    append_data = FALSE
  )
  non_na <- result[!is.na(result$distance_km), ]
  expect_true(all(non_na$distance_km <= 5))
})

test_that("threshold with very large distance returns all locations", {
  data(meck_ev, package = "Rvoterdistance")
  small_voters <- voter_meck[1:3, ]
  result <- nearest_location(small_voters, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    max_dist = 50000, units = "km",
    append_data = FALSE
  )
  non_na <- result[!is.na(result$location_id), ]
  expect_equal(nrow(non_na), 3 * nrow(early_meck))
})

test_that("threshold with zero distance returns empty or exact matches", {
  data(meck_ev, package = "Rvoterdistance")
  result <- nearest_location(voter_meck[1:5, ], early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    max_dist = 0, units = "km",
    append_data = FALSE
  )
  # All distances should be NA (no exact location matches expected)
  # or 0 (if voter is at an early vote location)
  non_na <- result[!is.na(result$distance_km), ]
  if (nrow(non_na) > 0) {
    expect_true(all(non_na$distance_km == 0))
  }
})
