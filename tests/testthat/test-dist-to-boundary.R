test_that("dist_to_boundary works with a simple linestring", {
  skip_if_not_installed("sf")
  library(sf)

  # Vertical line at lon = -109.05 (AZ-NM border, simplified)
  border <- st_sf(
    geometry = st_sfc(
      st_linestring(matrix(c(-109.05, 31.0, -109.05, 37.0),
        ncol = 2, byrow = TRUE
      )),
      crs = 4326
    )
  )

  # A voter east of the line
  voters <- data.frame(lat = 35.0, lon = -108.0)
  d <- dist_to_boundary(voters, border,
    voter_coords = c("lat", "lon"),
    units = "km", progress = FALSE
  )

  expect_length(d, 1)
  # ~108.0 - (-109.05) = 1.05 degrees longitude at lat 35
  # 1 degree longitude at lat 35 ≈ 111.32 * cos(35°) ≈ 91.2 km
  # So expect roughly 1.05 * 91.2 ≈ 95.7 km
  expect_true(d > 80 && d < 120)
})


test_that("dist_to_boundary works with a polygon", {
  skip_if_not_installed("sf")
  library(sf)

  # Simple rectangular polygon
  poly <- st_sf(
    geometry = st_sfc(
      st_polygon(list(matrix(c(
        -110, 35,
        -108, 35,
        -108, 37,
        -110, 37,
        -110, 35
      ), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  # Point inside the polygon — should get positive distance to edge
  voters_inside <- data.frame(lat = 36.0, lon = -109.0)
  d_inside <- dist_to_boundary(voters_inside, poly,
    voter_coords = c("lat", "lon"),
    units = "km", progress = FALSE
  )

  expect_length(d_inside, 1)
  expect_true(d_inside > 0)

  # Point is 1 degree from the nearest edge (lon -108 or -110)
  # 1 degree lon at lat 36 ≈ 111.32 * cos(36°) ≈ 90.1 km
  expect_true(d_inside > 70 && d_inside < 110)

  # Point outside the polygon
  voters_outside <- data.frame(lat = 36.0, lon = -107.0)
  d_outside <- dist_to_boundary(voters_outside, poly,
    voter_coords = c("lat", "lon"),
    units = "km", progress = FALSE
  )

  expect_true(d_outside > d_inside)
})


test_that("dist_to_boundary works with MULTILINESTRING", {
  skip_if_not_installed("sf")
  library(sf)

  mls <- st_sf(
    geometry = st_sfc(
      st_multilinestring(list(
        matrix(c(-109, 35, -109, 37), ncol = 2, byrow = TRUE),
        matrix(c(-107, 35, -107, 37), ncol = 2, byrow = TRUE)
      )),
      crs = 4326
    )
  )

  # Point between the two lines at lon = -108
  voters <- data.frame(lat = 36.0, lon = -108.0)
  d <- dist_to_boundary(voters, mls,
    voter_coords = c("lat", "lon"),
    units = "km", progress = FALSE
  )

  expect_length(d, 1)
  # Equidistant from both lines (1 degree each)
  # 1 degree lon at lat 36 ≈ 90 km
  expect_true(d > 70 && d < 110)
})


test_that("dist_to_boundary handles multiple voters", {
  skip_if_not_installed("sf")
  library(sf)

  border <- st_sf(
    geometry = st_sfc(
      st_linestring(matrix(c(-109.05, 31.0, -109.05, 37.0),
        ncol = 2, byrow = TRUE
      )),
      crs = 4326
    )
  )

  voters <- data.frame(
    lat = c(35.0, 35.0, 35.0),
    lon = c(-108.0, -109.05, -110.0)
  )
  d <- dist_to_boundary(voters, border,
    voter_coords = c("lat", "lon"),
    units = "km", progress = FALSE
  )

  expect_length(d, 3)
  # Voter 2 is on the line → distance should be ~0
  expect_true(d[2] < 1)
  # Voters 1 and 3 are symmetric → distances should be similar
  expect_true(abs(d[1] - d[3]) < 10)
})


test_that("dist_to_boundary returns correct units", {
  skip_if_not_installed("sf")
  library(sf)

  border <- st_sf(
    geometry = st_sfc(
      st_linestring(matrix(c(-109.05, 35.0, -109.05, 37.0),
        ncol = 2, byrow = TRUE
      )),
      crs = 4326
    )
  )

  voters <- data.frame(lat = 36.0, lon = -108.0)
  d_km <- dist_to_boundary(voters, border,
    voter_coords = c("lat", "lon"),
    units = "km", progress = FALSE
  )
  d_mi <- dist_to_boundary(voters, border,
    voter_coords = c("lat", "lon"),
    units = "miles", progress = FALSE
  )
  d_m <- dist_to_boundary(voters, border,
    voter_coords = c("lat", "lon"),
    units = "meters", progress = FALSE
  )

  expect_equal(d_m, d_km * 1000, tolerance = 0.01)
  expect_equal(d_m, d_mi * 1609.34, tolerance = 0.01)
})


test_that("dist_to_boundary works with sf POINT voters", {
  skip_if_not_installed("sf")
  library(sf)

  border <- st_sf(
    geometry = st_sfc(
      st_linestring(matrix(c(-109.05, 35.0, -109.05, 37.0),
        ncol = 2, byrow = TRUE
      )),
      crs = 4326
    )
  )

  voters_sf <- st_sf(
    id = 1,
    geometry = st_sfc(st_point(c(-108.0, 36.0)), crs = 4326)
  )

  d <- dist_to_boundary(voters_sf, border,
    units = "km", progress = FALSE
  )

  expect_length(d, 1)
  expect_true(d > 70 && d < 110)
})


test_that("dist_to_boundary accepts sfc boundary", {
  skip_if_not_installed("sf")
  library(sf)

  border_sfc <- st_sfc(
    st_linestring(matrix(c(-109.05, 35.0, -109.05, 37.0),
      ncol = 2, byrow = TRUE
    )),
    crs = 4326
  )

  voters <- data.frame(lat = 36.0, lon = -108.0)
  d <- dist_to_boundary(voters, border_sfc,
    voter_coords = c("lat", "lon"),
    units = "km", progress = FALSE
  )

  expect_length(d, 1)
  expect_true(d > 70 && d < 110)
})


test_that("dist_to_boundary rejects invalid boundary", {
  skip_if_not_installed("sf")
  library(sf)

  voters <- data.frame(lat = 36.0, lon = -108.0)

  # Non-sf object
  expect_error(
    dist_to_boundary(voters, data.frame(x = 1),
      voter_coords = c("lat", "lon")
    ),
    "sf or sfc"
  )

  # POINT geometry
  pt <- st_sf(geometry = st_sfc(st_point(c(-109, 36)), crs = 4326))
  expect_error(
    dist_to_boundary(voters, pt, voter_coords = c("lat", "lon")),
    "LINESTRING|POLYGON"
  )
})
