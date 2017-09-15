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
  IntegerVector vec(3);
  int n = i;
  for (int j = 3; j > 0; j--) {
    int c = j - 1;
    while (choose_C(c + 1, j) <= n) {
      c++;
    }
    vec(3 - j) = c;
    n = n - choose_C(c, j);
  }
  return vec;
}

//' @rdname combinatorial_bijections
//' @export
// [[Rcpp::export]]
int subset_index(IntegerVector vec) {
  if (vec.size() != 3) {
    stop("Vector 'vec' must be of length 3.");
  }
  if ((vec[0] <= vec[1]) | (vec[0] <= vec[2]) | (vec[1] <= vec[2])) {
    stop("Vector 'vec' must be strictly decreasing.");
  }
  if (vec[2] < 0) {
    stop("Vector 'vec' must be non-negative.");
  }
  int i = 0;
  for (int j = 0; j < 3; j++) {
    i += choose_C(vec[j], 3 - j);
  }
  return i;
}

//' @rdname combinatorial_bijections
//' @export
// [[Rcpp::export]]
IntegerVector subset_partition(IntegerVector vec) {
  if (vec.size() != 3) {
    stop("Vector 'vec' must be of length 3.");
  }
  if ((vec[0] <= vec[1]) | (vec[0] <= vec[2]) | (vec[1] <= vec[2])) {
    stop("Vector 'vec' must be strictly decreasing.");
  }
  if (vec[2] < 0) {
    stop("Vector 'vec' must be non-negative.");
  }
  IntegerVector lambda(3);
  for (int j = 0; j < 3; j++) {
    lambda[j] = vec[j] - 2 + j;
  }
  return lambda;
}

//' @rdname combinatorial_bijections
//' @export
// [[Rcpp::export]]
IntegerVector partition_subset(IntegerVector lambda) {
  if (lambda.size() != 3) {
    stop("Partition 'lambda' must have 3 parts.");
  }
  if ((lambda[0] < lambda[1]) |
      (lambda[0] < lambda[2]) |
      (lambda[1] < lambda[2])) {
    stop("Partition 'lambda' must be non-increasing.");
  }
  if (lambda[2] < 0) {
    stop("Partition 'lambda' must have non-negative parts.");
  }
  IntegerVector vec(3);
  for (int j = 0; j < 3; j++) {
    vec[j] = lambda[j] + 2 - j;
  }
  return vec;
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
int partition_index(IntegerVector lambda) {
  return subset_index(partition_subset(lambda));
}
