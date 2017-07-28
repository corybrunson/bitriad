// #include <iostream>
// #include <vector>
#include <Rcpp.h>
using namespace Rcpp;

//' Combinatorial bijections for affiliation network triad indexing
//' 
//' These functions biject among partitions of at most 3 parts, 3-subsets of
//' natural numbers, and indices for the lexicographic total orders on them.
//' 
//' @param i Integer; an index in the total order, starting at 0.
//' @param vec Integer; a set of 3 distinct non-negative integers, in decreasing
//'   order.
//' @param lambda Integer; a partition of at most 3 parts, with parts in 
//'   non-increasing order.
//' @examples
//' index_subset_C(2)
//' index_partition_C(2)
//' subset_index_C(c(3, 2, 0))
//' subset_partition_C(c(3, 2, 0))
//' partition_index_C(c(1, 1, 0))
//' partition_subset_C(c(1, 1, 0))

// https://stackoverflow.com/a/9331125
unsigned choose_C(unsigned n, unsigned k) {
  
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

//' @rdname combinatorial_bijections
//' @export
// [[Rcpp::export]]
IntegerVector index_subset_C(int i) {
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
int subset_index_C(IntegerVector vec) {
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
IntegerVector subset_partition_C(IntegerVector vec) {
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
IntegerVector partition_subset_C(IntegerVector lambda) {
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
IntegerVector index_partition_C(int i) {
  return subset_partition_C(index_subset_C(i));
}

//' @rdname combinatorial_bijections
//' @export
// [[Rcpp::export]]
int partition_index_C(IntegerVector lambda) {
  return subset_index_C(partition_subset_C(lambda));
}
