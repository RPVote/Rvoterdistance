#include <Rcpp.h>
#include <cmath>
#include <algorithm>
#include <vector>
#include <limits>

using namespace Rcpp;

// Forward declarations (defined in haversine.cpp)
double haversine_distance(double lat1, double lon1, double lat2, double lon2);

static const double EARTH_RADIUS_M = 6378137.0;
static const double PI = 3.14159265358979323846;
static const double DEG_TO_RAD = PI / 180.0;

// ── Internal helpers ─────────────────────────────────────────────────

// Bearing from point (lat1, lon1) to point (lat2, lon2) in radians
// All inputs in radians
static double bearing_rad(double lat1, double lon1, double lat2, double lon2) {
  double dlon = lon2 - lon1;
  double y = sin(dlon) * cos(lat2);
  double x = cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(dlon);
  return atan2(y, x);
}

// Distance from a point P to a great-circle arc segment A→B
// All inputs in degrees; returns distance in meters
static double point_to_segment_distance(
    double plat, double plon,
    double alat, double alon,
    double blat, double blon) {

  // Distance from A to P
  double d_ap = haversine_distance(alat, alon, plat, plon);

  // Degenerate segment: A == B
  double d_ab = haversine_distance(alat, alon, blat, blon);
  if (d_ab < 1e-10) return d_ap;

  // Convert to radians for bearing computation
  double alat_r = alat * DEG_TO_RAD;
  double alon_r = alon * DEG_TO_RAD;
  double blat_r = blat * DEG_TO_RAD;
  double blon_r = blon * DEG_TO_RAD;
  double plat_r = plat * DEG_TO_RAD;
  double plon_r = plon * DEG_TO_RAD;

  // Bearings from A
  double theta_ab = bearing_rad(alat_r, alon_r, blat_r, blon_r);
  double theta_ap = bearing_rad(alat_r, alon_r, plat_r, plon_r);

  // Cross-track distance (signed)
  double angular_ap = d_ap / EARTH_RADIUS_M;
  double sin_dxt = sin(angular_ap) * sin(theta_ap - theta_ab);

  // Clamp to valid range for asin
  if (sin_dxt > 1.0) sin_dxt = 1.0;
  if (sin_dxt < -1.0) sin_dxt = -1.0;

  double d_xt = asin(sin_dxt) * EARTH_RADIUS_M;

  // Along-track distance from A toward B
  double cos_angular_ap = cos(angular_ap);
  double cos_dxt_angular = cos(d_xt / EARTH_RADIUS_M);

  // Guard against division by zero or domain errors
  double ratio = cos_angular_ap / cos_dxt_angular;
  if (ratio > 1.0) ratio = 1.0;
  if (ratio < -1.0) ratio = -1.0;

  double d_at = acos(ratio) * EARTH_RADIUS_M;

  // Check if the perpendicular projection falls within the segment
  if (d_at >= 0.0 && d_at <= d_ab) {
    return fabs(d_xt);
  }

  // Otherwise, return distance to the closer endpoint
  double d_bp = haversine_distance(blat, blon, plat, plon);
  return std::min(d_ap, d_bp);
}

// Struct for pre-computed segment bounding boxes
struct SegBBox {
  double min_lat, max_lat, min_lon, max_lon;
};

// Quick lower bound on distance from point to a bounding box
// Uses simple haversine to the nearest point on the bbox boundary
// Returns a conservative (possibly underestimated) lower bound in meters
static double min_bbox_distance(double plat, double plon, const SegBBox& bb) {
  // If point is inside the bbox, lower bound is 0
  if (plat >= bb.min_lat && plat <= bb.max_lat &&
      plon >= bb.min_lon && plon <= bb.max_lon) {
    return 0.0;
  }

  // Clamp point to nearest edge of bbox
  double clat = plat;
  double clon = plon;
  if (clat < bb.min_lat) clat = bb.min_lat;
  if (clat > bb.max_lat) clat = bb.max_lat;
  if (clon < bb.min_lon) clon = bb.min_lon;
  if (clon > bb.max_lon) clon = bb.max_lon;

  return haversine_distance(plat, plon, clat, clon);
}

// ── Exported function ────────────────────────────────────────────────

// Compute minimum distance from each voter to the nearest segment
// of a boundary line/polygon.
//
// voter_lat, voter_lon:         coordinates of voters (degrees)
// seg_start_lat, seg_start_lon: start points of boundary segments (degrees)
// seg_end_lat, seg_end_lon:     end points of boundary segments (degrees)
// show_progress:                whether to print progress
//
// Returns NumericVector of distances in meters (one per voter)
//
// [[Rcpp::export]]
NumericVector cpp_dist_to_boundary(
    NumericVector voter_lat, NumericVector voter_lon,
    NumericVector seg_start_lat, NumericVector seg_start_lon,
    NumericVector seg_end_lat, NumericVector seg_end_lon,
    bool show_progress) {

  int n_voters = voter_lat.length();
  int n_segs = seg_start_lat.length();

  NumericVector result(n_voters);

  // Pre-compute bounding boxes for all segments
  std::vector<SegBBox> bboxes(n_segs);
  for (int j = 0; j < n_segs; j++) {
    bboxes[j].min_lat = std::min(seg_start_lat[j], seg_end_lat[j]);
    bboxes[j].max_lat = std::max(seg_start_lat[j], seg_end_lat[j]);
    bboxes[j].min_lon = std::min(seg_start_lon[j], seg_end_lon[j]);
    bboxes[j].max_lon = std::max(seg_start_lon[j], seg_end_lon[j]);
  }

  int progress_interval = std::max(1, n_voters / 50);

  for (int i = 0; i < n_voters; i++) {

    if (show_progress && (i % progress_interval == 0)) {
      Rcpp::Rcout << "\r  Processing voter " << (i + 1)
                  << " / " << n_voters << std::flush;
      Rcpp::checkUserInterrupt();
    }

    double min_dist = std::numeric_limits<double>::infinity();
    double vlat = voter_lat[i];
    double vlon = voter_lon[i];

    for (int j = 0; j < n_segs; j++) {
      // Bounding box pruning: skip if bbox is farther than current min
      double lb = min_bbox_distance(vlat, vlon, bboxes[j]);
      if (lb >= min_dist) continue;

      double d = point_to_segment_distance(
        vlat, vlon,
        seg_start_lat[j], seg_start_lon[j],
        seg_end_lat[j], seg_end_lon[j]
      );

      if (d < min_dist) {
        min_dist = d;
      }
    }

    result[i] = min_dist;
  }

  if (show_progress) {
    Rcpp::Rcout << "\r  Done: " << n_voters
                << " voters processed.           " << std::endl;
  }

  return result;
}
