#include <Rcpp.h>
#include <cmath>
#include <algorithm>
#include <vector>
#include <utility>

using namespace Rcpp;

// Forward declaration (defined in haversine.cpp)
double haversine_distance(double lat1, double lon1, double lat2, double lon2);

// Compute k-nearest locations for each voter
// Returns List with:
//   distances: NumericMatrix (n_voters x k)
//   indices:   IntegerMatrix (n_voters x k), 1-based
// [[Rcpp::export]]
List cpp_k_nearest(NumericVector voter_lat, NumericVector voter_lon,
                   NumericVector loc_lat, NumericVector loc_lon,
                   int k, bool show_progress) {

  int n_voters = voter_lat.length();
  int n_locs   = loc_lat.length();

  // Clamp k to number of locations
  if (k > n_locs) k = n_locs;

  NumericMatrix dist_out(n_voters, k);
  IntegerMatrix idx_out(n_voters, k);

  // Progress reporting: report every ~2% of voters
  int progress_interval = std::max(1, n_voters / 50);

  for (int i = 0; i < n_voters; i++) {

    if (show_progress && (i % progress_interval == 0)) {
      Rcpp::Rcout << "\r  Processing voter " << (i + 1) << " / " << n_voters << std::flush;
      Rcpp::checkUserInterrupt();
    }

    // Compute all distances for voter i
    std::vector<std::pair<double, int> > dists(n_locs);
    for (int j = 0; j < n_locs; j++) {
      dists[j] = std::make_pair(
        haversine_distance(voter_lat[i], voter_lon[i], loc_lat[j], loc_lon[j]),
        j
      );
    }

    // Partial sort: only need the k smallest
    std::nth_element(dists.begin(), dists.begin() + k, dists.end());
    // Sort the first k elements for deterministic ordering
    std::sort(dists.begin(), dists.begin() + k);

    for (int m = 0; m < k; m++) {
      dist_out(i, m) = dists[m].first;
      idx_out(i, m)  = dists[m].second + 1;  // 1-based for R
    }
  }

  if (show_progress) {
    Rcpp::Rcout << "\r  Done: " << n_voters << " voters processed.           " << std::endl;
  }

  return List::create(
    Named("distances") = dist_out,
    Named("indices")   = idx_out
  );
}


// Threshold search: return all locations within max_dist_m (meters) for each voter
// Returns a List of length n_voters, each element a List with:
//   distances: NumericVector
//   indices:   IntegerVector (1-based)
// [[Rcpp::export]]
List cpp_within_threshold(NumericVector voter_lat, NumericVector voter_lon,
                          NumericVector loc_lat, NumericVector loc_lon,
                          double max_dist_m, bool show_progress) {

  int n_voters = voter_lat.length();
  int n_locs   = loc_lat.length();

  List result(n_voters);
  int progress_interval = std::max(1, n_voters / 50);

  for (int i = 0; i < n_voters; i++) {

    if (show_progress && (i % progress_interval == 0)) {
      Rcpp::Rcout << "\r  Processing voter " << (i + 1) << " / " << n_voters << std::flush;
      Rcpp::checkUserInterrupt();
    }

    std::vector<double> dists;
    std::vector<int> idxs;

    for (int j = 0; j < n_locs; j++) {
      double d = haversine_distance(voter_lat[i], voter_lon[i], loc_lat[j], loc_lon[j]);
      if (d <= max_dist_m) {
        dists.push_back(d);
        idxs.push_back(j + 1);  // 1-based for R
      }
    }

    result[i] = List::create(
      Named("distances") = wrap(dists),
      Named("indices")   = wrap(idxs)
    );
  }

  if (show_progress) {
    Rcpp::Rcout << "\r  Done: " << n_voters << " voters processed.           " << std::endl;
  }

  return result;
}
