#include <Rcpp.h>

using namespace Rcpp;

// https://stackoverflow.com/a/9331125
int choose_C(int n, int k) {
  
  if (k > n) return 0;
  if (k * 2 > n) k = n - k;
  if (k == 0) return 1;
  
  int res = n;
  for (int i = 2; i <= k; ++i) {
    res *= (n - i + 1);
    res /= i;
  }
  
  return res;
}

// Combinatorial bijections for full triad census indexing

//' @rdname combinatorial_bijections
//' @export
// [[Rcpp::export]]
IntegerVector index_subset(int i) {
  if (i < 0) {
    stop("Index 'i' must be a non-negative integer.");
  }
  IntegerVector v(3);
  int n = i;
  for (int j = 3; j > 0; j--) {
    int c = j - 1;
    while (choose_C(c + 1, j) <= n) {
      c++;
    }
    v(3 - j) = c;
    n = n - choose_C(c, j);
  }
  return v;
}

//' @rdname combinatorial_bijections
//' @export
// [[Rcpp::export]]
int subset_index(IntegerVector v) {
  if (v.size() != 3) {
    stop("Vector 'v' must be of length 3.");
  }
  if ((v[0] <= v[1]) | (v[0] <= v[2]) | (v[1] <= v[2])) {
    stop("Vector 'v' must be strictly decreasing.");
  }
  if (v[2] < 0) {
    stop("Vector 'v' must be non-negative.");
  }
  int i = 0;
  for (int j = 0; j < 3; j++) {
    i += choose_C(v[j], 3 - j);
  }
  return i;
}

//' @rdname combinatorial_bijections
//' @export
// [[Rcpp::export]]
IntegerVector subset_partition(IntegerVector v) {
  if (v.size() != 3) {
    stop("Vector 'v' must be of length 3.");
  }
  if ((v[0] <= v[1]) | (v[0] <= v[2]) | (v[1] <= v[2])) {
    stop("Vector 'v' must be strictly decreasing.");
  }
  if (v[2] < 0) {
    stop("Vector 'v' must be non-negative.");
  }
  IntegerVector par(3);
  for (int j = 0; j < 3; j++) {
    par[j] = v[j] - 2 + j;
  }
  return par;
}

//' @rdname combinatorial_bijections
//' @export
// [[Rcpp::export]]
IntegerVector partition_subset(IntegerVector par) {
  if (par.size() != 3) {
    stop("Partition 'par' must have 3 parts.");
  }
  if ((par[0] < par[1]) |
      (par[0] < par[2]) |
      (par[1] < par[2])) {
    stop("Partition 'par' must be non-increasing.");
  }
  if (par[2] < 0) {
    stop("Partition 'par' must have non-negative parts.");
  }
  IntegerVector v(3);
  for (int j = 0; j < 3; j++) {
    v[j] = par[j] + 2 - j;
  }
  return v;
}

//' @rdname combinatorial_bijections
//' @export
// [[Rcpp::export]]
IntegerVector index_partition(int i) {
  return subset_partition(index_subset(i));
}

//' @rdname combinatorial_bijections
//' @export
// [[Rcpp::export]]
int partition_index(IntegerVector par) {
  return subset_index(partition_subset(par));
}
