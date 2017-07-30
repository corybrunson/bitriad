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

List actor_nbhd_1(IntegerMatrix el, int q) {
  
  int m = el.nrow();
  
  // Identify the sphere of radius 1 about q
  std::vector<int> n1;
  int i;
  for (i = 0; i < m; i++) {
    if (el(i, 0) == q) {
      if (std::find(n1.begin(), n1.end(), el(i, 1)) != n1.end()) {
        continue;
      } else {
        n1.push_back(el(i, 1));
      }
    }
  }
  
  return List::create(Named("d0") = q,
                      Named("d1") = n1);
}

List event_nbhd_1(IntegerMatrix el, int a) {
  
  int m = el.nrow();
  
  // Identify the sphere of radius 1 about a
  std::vector<int> n1;
  int i;
  for (i = 0; i < m; i++) {
    if (el(i, 1) == a) {
      if (std::find(n1.begin(), n1.end(), el(i, 0)) != n1.end()) {
        continue;
      } else {
        n1.push_back(el(i, 0));
      }
    }
  }
  
  return List::create(Named("d0") = a,
                      Named("d1") = n1);
}

List actor_nbhd_2(IntegerMatrix el, int q) {
  
  int m = el.nrow();
  
  // Identify the spheres of radius 1 and 2 about q
  std::vector<int> n1;
  std::vector<int> n2;
  int i, j;
  for (i = 0; i < m; i++) {
    if (el(i, 0) == q) {
      if (std::find(n1.begin(), n1.end(), el(i, 1)) != n1.end()) {
        continue;
      } else {
        n1.push_back(el(i, 1));
        for (j = 0; j < m; j++) {
          if (j == i) {
            continue;
          }
          if (el(j, 1) == el(i, 1)) {
            if (std::find(n2.begin(), n2.end(), el(j, 0)) != n2.end()) {
              continue;
            } else {
              n2.push_back(el(j, 0));
            }
          }
        }
      }
    }
  }
  
  return List::create(Named("d0") = q,
                      Named("d1") = n1,
                      Named("d2") = n2);
}

unsigned tetrahedral_C(unsigned n) {
  return choose_C(n + 3, 3);
}

// Triad census for affiliation networks

// algorithm adapted from Batagelj and Mrvar (2001)
// performed on an edgelist
// [[Rcpp::export]]
IntegerMatrix triad_census_edgelist(
    IntegerMatrix el,
    IntegerVector actors,
    int max_weight
) {
  
  // Loop indices
  unsigned i, j, k;
  // Counters
  IntegerVector lambda(3);
  unsigned w;
  // Limits
  unsigned lambda_i = 0;
  unsigned max_i = 0;
  unsigned max_w = 0;
  
  // Initialize triad census matrix
  IntegerMatrix tc(tetrahedral_C(max_weight), max_weight + 1);
  
  std::sort(actors.begin(), actors.end());
  
  // Loop over actors p
  for (i = 0; i < actors.size(); i++) {
    
    List p_ego = actor_nbhd_2(el, actors[i]);
    IntegerVector p_events = p_ego["d1"];
    std::sort(p_events.begin(), p_events.end());
    IntegerVector p_actors = p_ego["d2"];
    std::sort(p_actors.begin(), p_actors.end());
    
    // Actors q co-incident with actor p
    IntegerVector actors_q = p_actors;
    
    for (j = 0; j < actors_q.size(); j++) {
      if (actors_q[j] <= actors[i]) {
        continue;
      }
      
      List q_ego = actor_nbhd_2(el, actors_q[j]);
      IntegerVector q_events = q_ego["d1"];
      std::sort(q_events.begin(), q_events.end());
      IntegerVector q_actors = q_ego["d2"];
      std::sort(q_actors.begin(), q_actors.end());
      
      // Events a attended by actors p and q
      IntegerVector events_a = IntegerVector::create();
      std::set_intersection(p_events.begin(), p_events.end(),
                            q_events.begin(), q_events.end(),
                            std::back_inserter(events_a));
      
      // Actors r co-incident with either actor p or q
      // (correspond to set S in Batagelj-Mrvar, without excluding p and q)
      std::vector<int> actors_r(p_actors.size() + q_actors.size());
      std::vector<int>::iterator it;
      it = std::set_union(p_actors.begin(), p_actors.end(),
                          q_actors.begin(), q_actors.end(),
                          actors_r.begin());
      actors_r.resize(it - actors_r.begin());
      std::sort(actors_r.begin(), actors_r.end());
      
      // Tally one-link triads
      // Partition of exclusive event counts lambda
      lambda[0] = events_a.size();
      lambda[1] = 0;
      lambda[2] = 0;
      lambda_i = partition_index_C(lambda);
      max_i = std::max(max_i, lambda_i);
      // Inclusive event count w
      w = 0;
      // Increment matrix entry
      tc(lambda_i, w) += actors.size() - actors_r.size();
      
      for (k = 0; k < actors_r.size(); k++) {
        if ((actors_r[k] == actors[i]) |
            (actors_r[k] == actors_q[j])) {
          continue;
        }
        
        List r_ego = actor_nbhd_2(el, actors_r[k]);
        IntegerVector r_events = r_ego["d1"];
        std::sort(r_events.begin(), r_events.end());
        IntegerVector r_actors = r_ego["d2"];
        std::sort(r_actors.begin(), r_actors.end());
        
        // Events c attended by actors p and r
        IntegerVector events_c = IntegerVector::create();
        std::set_intersection(p_events.begin(), p_events.end(),
                              r_events.begin(), r_events.end(),
                              std::back_inserter(events_c));
        
        if ((actors_q[j] >= actors_r[k]) &
            ((actors[i] >= actors_r[k]) |
            (actors_r[k] >= actors_q[j]) |
            (events_c.size() > 0))) {
          continue;
        }
        
        // Events b attended by actors q and r
        IntegerVector events_b = IntegerVector::create();
        std::set_intersection(q_events.begin(), q_events.end(),
                              r_events.begin(), r_events.end(),
                              std::back_inserter(events_b));
        // Events d attended by actors p, q, and r
        IntegerVector events_d = IntegerVector::create();
        std::set_intersection(events_b.begin(), events_b.end(),
                              events_c.begin(), events_c.end(),
                              std::back_inserter(events_d));
        
        // Tally two- and three-link triads
        // Partition of exclusive event counts lambda
        lambda[0] = events_a.size() - events_d.size();
        lambda[1] = events_b.size() - events_d.size();
        lambda[2] = events_c.size() - events_d.size();
        std::sort(lambda.begin(), lambda.end());
        std::reverse(lambda.begin(), lambda.end());
        lambda_i = partition_index_C(lambda);
        max_i = std::max(max_i, lambda_i);
        // Inclusive event count w
        w = events_d.size();
        max_w = std::max(max_w, w);
        // Increment matrix entry
        tc(lambda_i, w) += 1;
        
      }
    }
  }
  
  // Tally zero-link triads
  // Partition of exclusive event counts lambda
  lambda_i = 0;
  // Inclusive event count w
  w = 0;
  // Count non-zero triads
  int tot = 0;
  for (i = 0; i < max_i + 1; i++) {
    for (j = 0; j < max_w + 1; j++) {
      tot += tc(i, j);
    }
  }
  // Increment matrix entry
  tc(lambda_i, w) += (choose_C(actors.size(), 3) - tot);
  
  // Subset matrix according to 'max_i' and 'max_w'
  tc = tc(Range(0, max_i + 1), Range(0, max_w));
  
  return tc;
}
