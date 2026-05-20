#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

static const double EARTH_RADIUS_M = 6378137.0;  // WGS-84 equatorial radius in meters
static const double PI = 3.14159265358979323846;

// Internal: convert degrees to radians
static double deg2rad(double deg) {
  return deg * PI / 180.0;
}

// Haversine (great-circle) distance between two lat/lon points
// Returns distance in meters
// [[Rcpp::export]]
double haversine_distance(double lat1, double lon1, double lat2, double lon2) {
  double lat1r = deg2rad(lat1);
  double lon1r = deg2rad(lon1);
  double lat2r = deg2rad(lat2);
  double lon2r = deg2rad(lon2);
  double u = sin((lat2r - lat1r) / 2.0);
  double v = sin((lon2r - lon1r) / 2.0);
  return 2.0 * EARTH_RADIUS_M * asin(sqrt(u * u + cos(lat1r) * cos(lat2r) * v * v));
}
