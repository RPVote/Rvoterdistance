# Rvoterdistance
[![R build status](https://github.com/RPVote/Rvoterdistance/workflows/R-CMD-check/badge.svg)](https://github.com/RPVote/Rvoterdistance/actions?workflow=R-CMD-check)
[![Style status](https://github.com/RPVote/Rvoterdistance/workflows/Styler/badge.svg)](https://github.com/RPVote/Rvoterdistance/actions?workflow=Styler)

Calculates the geographic distance between voters in a voter file and multiple polling or vote-by-mail drop box locations using the Haversine (great-circle) formula. Core computation is implemented in C++ via Rcpp for speed.

## Features

- **Nearest location**: find the single closest polling place for each voter
- **k-nearest locations**: find the k closest locations per voter
- **Distance threshold**: find all locations within a specified radius (e.g., all drop boxes within 5 miles)
- **sf integration**: pass `sf` POINT geometries directly with automatic CRS transformation to WGS-84
- **Progress reporting**: optional progress output for large voter files
- **Units**: results in meters, kilometers, and miles

## Installation

```r
# From GitHub:
remotes::install_github("lorenc5/Rvoterdistance")
```

## Quick Start

```r
library(Rvoterdistance)
data(meck_ev)

# Nearest early voting location for each voter
result <- nearest_location(voter_meck, early_meck,
  voter_coords = c("lat", "long"),
  location_coords = c("lat", "long"))
head(result)

# 3 nearest locations per voter
result_k3 <- nearest_location(voter_meck, early_meck,
  voter_coords = c("lat", "long"),
  location_coords = c("lat", "long"),
  k = 3)

# All locations within 5 miles
result_5mi <- nearest_location(voter_meck, early_meck,
  voter_coords = c("lat", "long"),
  location_coords = c("lat", "long"),
  max_dist = 5, units = "miles")
```

## Main Functions

| Function | Description |
|---|---|
| `nearest_location()` | Main entry point. Supports k-nearest, distance threshold, sf objects. |
| `dist_km()` | Minimum distance to nearest location in kilometers. |
| `dist_mile()` | Minimum distance to nearest location in miles. |
| `haversine()` | Single-pair great-circle distance with configurable units. |

## Using sf Objects

```r
library(sf)
voters_sf <- st_as_sf(voter_meck, coords = c("long", "lat"), crs = 4326)
locs_sf   <- st_as_sf(early_meck, coords = c("long", "lat"), crs = 4326)

result <- nearest_location(voters_sf, locs_sf)
```

If the CRS is not WGS-84 (EPSG:4326), coordinates are automatically transformed.

## Included Data

- **king_dbox / king_geo**: King County, WA ballot drop box locations and voter sample
- **meck_ev (voter_meck / early_meck)**: Mecklenburg County, NC early voting locations and voter sample

## Citation

Collingwood, Loren. 2026. *Rvoterdistance: Voter Distance to Polling Locations*. R package version 2.0.0. https://github.com/lorenc5/Rvoterdistance

## License

GPL (>= 2)
