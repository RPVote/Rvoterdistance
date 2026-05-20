test_that("haversine distance matches known value (NYC to London)", {
  # Well-known reference: ~5570 km
  d <- haversine(40.7128, -74.0060, 51.5074, -0.1278, units = "km")
  expect_equal(d, 5570, tolerance = 20)
})

test_that("haversine distance is zero for same point", {
  d <- haversine(47.6062, -122.3321, 47.6062, -122.3321)
  expect_equal(d, 0)
})

test_that("haversine is symmetric", {
  d1 <- haversine(40.7128, -74.0060, 51.5074, -0.1278)
  d2 <- haversine(51.5074, -0.1278, 40.7128, -74.0060)
  expect_equal(d1, d2)
})

test_that("haversine respects units argument", {
  d_m  <- haversine(40.7128, -74.0060, 51.5074, -0.1278, units = "meters")
  d_km <- haversine(40.7128, -74.0060, 51.5074, -0.1278, units = "km")
  d_mi <- haversine(40.7128, -74.0060, 51.5074, -0.1278, units = "miles")
  expect_equal(d_km, d_m / 1000)
  expect_equal(d_mi, d_m / 1609.34)
})
