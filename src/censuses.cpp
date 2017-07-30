// #include <iostream>
// #include <vector>
#include <Rcpp.h>
using namespace Rcpp;

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

unsigned tetrahedral_C(unsigned n) {
  return choose_C(n + 3, 3);
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

// Wedge censuses and closure indicators

// alcove x:
//   0 <-> T_(1,1,1),0
//   1 <-> T_(1,1,0),1
//   2 <-> T_(1,0,0),2
//   3 <-> T_(0,0,0),3
// wedge w:
//   0 <-> T_(1,1,0),0
//   1 <-> T_(1,0,0),1
//   2 <-> T_(0,0,0),2
// graph maps m:
//   0 <-> all graph maps (injective on actors)
//   1 <-> injective graph maps
//   2 <-> induced injective graph maps
// congruence relations c:
//   0 <-> same actor images, same event images
//   1 <-> same actor images, structurally equivalent event images
//   2 <-> same actor images

// X = T_111,0; W = T_110,0
// all graph maps, modulo equal event images
// 4-paths p,a,q,b,r (with p,q,r distinct)
// and whether they are contained in 6-cycles
// [[Rcpp::export]]
List wedges_x0w0m0c0(IntegerMatrix el, int q) {
  
  // Loop indices
  int i,j,k,l;
  
  // Incident events
  IntegerVector q_events, a_actors, q_self, a_actors_q;
  // Compute event (distance 1) neigborhoods about q
  List q_ego = actor_nbhd_2(el, q);
  q_events = q_ego["d1"];
  std::sort(q_events.begin(), q_events.end());
  int q_events_count = q_events.size();
  // Compute actor (distance 1) neighborhoods about events of q, excluding q
  q_self = q;
  std::vector<IntegerVector> a_actors_q_list;
  for (i = 0; i < q_events_count; i++) {
    a_actors = event_nbhd_1(el, q_events[i])["d1"];
    a_actors_q = IntegerVector::create();
    std::set_difference(a_actors.begin(), a_actors.end(),
                        q_self.begin(), q_self.end(),
                        std::inserter(a_actors_q, a_actors_q.end()));
    std::sort(a_actors_q.begin(), a_actors_q.end());
    a_actors_q_list.push_back(a_actors_q);
  }
  
  // Initialize paired vectors of event and actor neighbors of q
  std::vector<int> p_vec;
  std::vector<int> a_vec;
  std::vector<int> b_vec;
  std::vector<int> r_vec;
  std::vector<bool> cl_vec;
  // Initialize intersections and differences
  std::vector<int> p_events;
  std::vector<int> r_events;
  std::vector<int> pr_events;
  // Add 4-paths (with distinct actors)
  bool cl;
  for (i = 0; i < q_events_count; i++) {
    for (j = i; j < q_events_count; j++) {
      for (k = 0; k < a_actors_q_list[i].size(); k++) {
        for (l = 0; l < a_actors_q_list[j].size(); l++) {
          // Ensure that actors are distinct
          if (a_actors_q_list[i][k] == a_actors_q_list[j][l]) {
            continue;
          }
          // If the events are equal, then ensure that the actors are ordered
          if ((q_events[i] == q_events[j]) &&
              (a_actors_q_list[i][k] > a_actors_q_list[j][l])) {
            continue;
          }
          // Keep the 4-path as a wedge
          p_vec.push_back(a_actors_q_list[i][k]);
          a_vec.push_back(q_events[i]);
          b_vec.push_back(q_events[j]);
          r_vec.push_back(a_actors_q_list[j][l]);
          // Calculate the event (distance 1) neighborhoods of p and r
          p_events = actor_nbhd_1(el, a_actors_q_list[i][k])["d1"];
          r_events = actor_nbhd_1(el, a_actors_q_list[j][l])["d1"];
          // Calculate the intersection of the events of p and of r
          pr_events.clear();
          std::set_intersection(p_events.begin(),
                                p_events.end(),
                                r_events.begin(),
                                r_events.end(),
                                std::back_inserter(pr_events));
          // If p and r share an event,
          // then the wedge is closed
          cl = (pr_events.size() > 0);
          cl_vec.push_back(cl);
        }
      }
    }
  }
  
  // Return integer matrix and closedness indicators
  IntegerMatrix wedges(5, p_vec.size());
  LogicalVector closed(p_vec.size());
  for (i = 0; i < p_vec.size(); i++) {
    wedges(0, i) = p_vec[i];
    wedges(1, i) = a_vec[i];
    wedges(2, i) = q;
    wedges(3, i) = b_vec[i];
    wedges(4, i) = r_vec[i];
    closed(i) = cl_vec[i];
  }
  return List::create(
    _["wedges"] = wedges,
    _["closed"] = closed
  );
}

// X = T_111,0; W = T_110,0
// all graph maps, modulo structurally equivalent event images
// pairs p,r of actor neighbors of q
// with exclusive or inclusive events shared with q
// and whether they share an exclusive or inclusive event
// [[Rcpp::export]]
List wedges_x0w0m0c1(IntegerMatrix el, int q) {
  
  // Loop indices
  int i,j;
  
  // Incident events
  IntegerVector q_actors, q_events, p_events, pq_events, p_q_events;
  // Compute event (distance 1) and actor (distance 2) neighborhoods about q
  List q_ego = actor_nbhd_2(el, q);
  q_events = q_ego["d1"];
  std::sort(q_events.begin(), q_events.end());
  q_actors = q_ego["d2"];
  std::sort(q_actors.begin(), q_actors.end());
  int q_actors_count = q_actors.size();
  
  // Compute event neighborhoods about actor neigbhors of q
  std::vector<IntegerVector> p_events_list;
  std::vector<IntegerVector> pq_events_list;
  for (i = 0; i < q_actors_count; i++) {
    p_events = actor_nbhd_1(el, q_actors[i])["d1"];
    std::sort(p_events.begin(), p_events.end());
    p_events_list.push_back(p_events);
    // Restrict to events shared with q
    pq_events = IntegerVector::create();
    std::set_intersection(q_events.begin(), q_events.end(),
                          p_events.begin(), p_events.end(),
                          std::back_inserter(pq_events));
    pq_events_list.push_back(pq_events);
  }
  
  // Initialize paired vectors of actor neighbors of q
  std::vector<int> p_vec;
  std::vector<int> r_vec;
  std::vector<bool> cl_vec;
  // Initialize intersections and differences
  std::vector<int> pr_events;
  std::vector<int> pqr_events;
  // Add pairs with mutually exclusive events
  bool cl;
  for (i = 0; i < q_actors_count - 1; i++) {
    for (j = i + 1; j < q_actors_count; j++) {
      // Calculate the intersection of the events shared by p and r
      pr_events.clear();
      std::set_intersection(p_events_list[i].begin(),
                            p_events_list[i].end(),
                            p_events_list[j].begin(),
                            p_events_list[j].end(),
                            std::back_inserter(pr_events));
      std::sort(pr_events.begin(), pr_events.end());
      // Calculate the intersection of the inclusive events shared by p and r
      pqr_events.clear();
      std::set_intersection(pq_events_list[i].begin(),
                            pq_events_list[i].end(),
                            pq_events_list[j].begin(),
                            pq_events_list[j].end(),
                            std::back_inserter(pqr_events));
      // Keep a wedge with exclusive events shared with p and r
      if ((pq_events_list[i].size() > pqr_events.size()) &&
          (pq_events_list[j].size() > pqr_events.size())) {
        p_vec.push_back(q_actors[i]);
        r_vec.push_back(q_actors[j]);
        // If p and r share an event,
        // then the wedge is closed
        cl = (pr_events.size() > 0);
        cl_vec.push_back(cl);
      }
      // Keep a wedge with an exclusive event with p and an inclusive one with r
      if ((pq_events_list[i].size() > pqr_events.size()) &&
          (pqr_events.size() > 0)) {
        p_vec.push_back(q_actors[i]);
        r_vec.push_back(q_actors[j]);
        // Since the inclusive event can double as the closing event,
        // the wedge is closed
        cl = (true);
        cl_vec.push_back(cl);
      }
      // Keep a wedge with an inclusive event with p and an exclusive one with r
      if ((pqr_events.size() > 0) &&
          (pq_events_list[j].size() > pqr_events.size())) {
        p_vec.push_back(q_actors[i]);
        r_vec.push_back(q_actors[j]);
        // Since the inclusive event can double as the closing event,
        // the wedge is closed
        cl = (true);
        cl_vec.push_back(cl);
      }
      // Keep a wedge with an inclusive event
      if (pqr_events.size() > 0) {
        p_vec.push_back(q_actors[i]);
        r_vec.push_back(q_actors[j]);
        // Since the inclusive event can triple as the closing event,
        // the wedge is closed
        cl = (true);
        cl_vec.push_back(cl);
      }
    }
  }
  
  // Return integer matrix and closedness indicators
  IntegerMatrix wedges(3, p_vec.size());
  LogicalVector closed(p_vec.size());
  for (i = 0; i < p_vec.size(); i++) {
    wedges(0, i) = p_vec[i];
    wedges(1, i) = q;
    wedges(2, i) = r_vec[i];
    closed(i) = cl_vec[i];
  }
  return List::create(
    _["wedges"] = wedges,
    _["closed"] = closed
  );
}

// X = T_111,0; W = T_110,0
// all graph maps, modulo all event images
// (classical wedges and closure)
// pairs p,r of actor neighbors of q
// and whether they share an event
// [[Rcpp::export]]
List wedges_x0w0m0c2(IntegerMatrix el, int q) {
  
  // Loop indices
  int i,j;
  
  // Incident events
  IntegerVector q_actors, q_events, p_events, pq_events;
  // Compute event (distance 1) and actor (distance 2) neighborhoods about q
  List q_ego = actor_nbhd_2(el, q);
  q_events = q_ego["d1"];
  std::sort(q_events.begin(), q_events.end());
  q_actors = q_ego["d2"];
  std::sort(q_actors.begin(), q_actors.end());
  int q_actors_count = q_actors.size();
  
  // Compute event neighborhoods about actor neigbhors of q
  std::vector<IntegerVector> p_events_list;
  for (i = 0; i < q_actors_count; i++) {
    p_events = actor_nbhd_1(el, q_actors[i])["d1"];
    std::sort(p_events.begin(), p_events.end());
    p_events_list.push_back(p_events);
  }
  
  // Initialize paired vectors of actor neighbors of q
  std::vector<int> p_vec;
  std::vector<int> r_vec;
  std::vector<bool> cl_vec;
  // Initialize intersection
  std::vector<int> pr_events;
  // Add all pairs
  bool cl;
  for (i = 0; i < q_actors_count - 1; i++) {
    for (j = i + 1; j < q_actors_count; j++) {
      p_vec.push_back(q_actors[i]);
      r_vec.push_back(q_actors[j]);
      // If intersection of p & r events is nonzero,
      // then the wedge is closed
      pr_events.clear();
      std::set_intersection(p_events_list[i].begin(),
                            p_events_list[i].end(),
                            p_events_list[j].begin(),
                            p_events_list[j].end(),
                            std::back_inserter(pr_events));
      cl = (pr_events.size() > 0);
      cl_vec.push_back(cl);
    }
  }
  
  // Return integer matrix and closedness indicators
  IntegerMatrix wedges(3, p_vec.size());
  LogicalVector closed(p_vec.size());
  for (i = 0; i < p_vec.size(); i++) {
    wedges(0, i) = p_vec[i];
    wedges(1, i) = q;
    wedges(2, i) = r_vec[i];
    closed(i) = cl_vec[i];
  }
  return List::create(
    _["wedges"] = wedges,
    _["closed"] = closed
  );
}

// X = T_111,0; W = T_110,0
// injective graph maps, modulo equal event images
// (Opsahl wedges and closure)
// 4-paths p,a,q,b,r (with p,q,r and a,b distinct)
// and whether they are contained in 6-cycles (with a,b,c distinct)
// [[Rcpp::export]]
List wedges_x0w0m1c0(IntegerMatrix el, int q) {
  
  // Loop indices
  int i,j,k,l;
  
  // Incident events
  IntegerVector q_events, a_actors, q_self, a_actors_q;
  // Compute event (distance 1) neigborhoods about q
  List q_ego = actor_nbhd_2(el, q);
  q_events = q_ego["d1"];
  std::sort(q_events.begin(), q_events.end());
  int q_events_count = q_events.size();
  // Compute actor (distance 1) neighborhoods about events of q, excluding q
  q_self = q;
  std::vector<IntegerVector> a_actors_q_list;
  for (i = 0; i < q_events_count; i++) {
    a_actors = event_nbhd_1(el, q_events[i])["d1"];
    a_actors_q = IntegerVector::create();
    std::set_difference(a_actors.begin(), a_actors.end(),
                        q_self.begin(), q_self.end(),
                        std::inserter(a_actors_q, a_actors_q.end()));
    std::sort(a_actors_q.begin(), a_actors_q.end());
    a_actors_q_list.push_back(a_actors_q);
  }
  
  // Initialize paired vectors of event and actor neighbors of q
  std::vector<int> p_vec;
  std::vector<int> a_vec;
  std::vector<int> b_vec;
  std::vector<int> r_vec;
  std::vector<bool> cl_vec;
  // Initialize intersections and differences
  std::vector<int> p_events;
  std::vector<int> r_events;
  std::vector<int> pr_events;
  std::vector<int> ab;
  std::vector<int> pr_events_ab;
  // Add 4-paths (with distinct nodes)
  bool cl;
  for (i = 0; i < q_events_count - 1; i++) {
    for (j = i + 1; j < q_events_count; j++) {
      // Calculate the union of a and b
      ab.clear();
      ab.push_back(q_events[i]);
      ab.push_back(q_events[j]);
      for (k = 0; k < a_actors_q_list[i].size(); k++) {
        for (l = 0; l < a_actors_q_list[j].size(); l++) {
          // Ensure that actors are distinct
          if (a_actors_q_list[i][k] == a_actors_q_list[j][l]) {
            continue;
          }
          // Keep the 4-path as a wedge
          p_vec.push_back(a_actors_q_list[i][k]);
          a_vec.push_back(q_events[i]);
          b_vec.push_back(q_events[j]);
          r_vec.push_back(a_actors_q_list[j][l]);
          // If p and r share an event other than a and b,
          // then the wedge is closed
          // Calculate the event (distance 1) neighborhoods of p and r
          p_events = actor_nbhd_1(el, a_actors_q_list[i][k])["d1"];
          r_events = actor_nbhd_1(el, a_actors_q_list[j][l])["d1"];
          // Calculate the intersection of the events of p and of r
          pr_events.clear();
          std::set_intersection(p_events.begin(),
                                p_events.end(),
                                r_events.begin(),
                                r_events.end(),
                                std::back_inserter(pr_events));
          // Calculate the difference of the shared events of p, r, and a, b
          pr_events_ab.clear();
          std::set_difference(pr_events.begin(), pr_events.end(),
                              ab.begin(), ab.end(),
                              std::inserter(pr_events_ab, pr_events_ab.end()));
          cl = (pr_events_ab.size() > 0);
          cl_vec.push_back(cl);
        }
      }
    }
  }
  
  // Return integer matrix and closedness indicators
  IntegerMatrix wedges(5, p_vec.size());
  LogicalVector closed(p_vec.size());
  for (i = 0; i < p_vec.size(); i++) {
    wedges(0, i) = p_vec[i];
    wedges(1, i) = a_vec[i];
    wedges(2, i) = q;
    wedges(3, i) = b_vec[i];
    wedges(4, i) = r_vec[i];
    closed(i) = cl_vec[i];
  }
  return List::create(
    _["wedges"] = wedges,
    _["closed"] = closed
  );
}

// X = T_111,0; W = T_110,0
// injective graph maps, modulo structurally equivalent event images
// pairs p,r of actor neighbors of q
// with distinct exclusive or inclusive events shared with q
// and whether they share an additional event
// [[Rcpp::export]]
List wedges_x0w0m1c1(IntegerMatrix el, int q) {
  
  // Loop indices
  int i,j;
  
  // Incident events
  IntegerVector q_actors, q_events, p_events, pq_events, p_q_events;
  // Compute event (distance 1) and actor (distance 2) neighborhoods about q
  List q_ego = actor_nbhd_2(el, q);
  q_events = q_ego["d1"];
  std::sort(q_events.begin(), q_events.end());
  q_actors = q_ego["d2"];
  std::sort(q_actors.begin(), q_actors.end());
  int q_actors_count = q_actors.size();
  
  // Compute event neighborhoods about actor neigbhors of q
  std::vector<IntegerVector> p_events_list;
  std::vector<IntegerVector> pq_events_list;
  std::vector<IntegerVector> p_q_events_list;
  for (i = 0; i < q_actors_count; i++) {
    p_events = actor_nbhd_1(el, q_actors[i])["d1"];
    std::sort(p_events.begin(), p_events.end());
    p_events_list.push_back(p_events);
    // Restrict to events shared with q
    pq_events = IntegerVector::create();
    std::set_intersection(q_events.begin(), q_events.end(),
                          p_events.begin(), p_events.end(),
                          std::back_inserter(pq_events));
    pq_events_list.push_back(pq_events);
    // Restrict to events not shared with q
    p_q_events = IntegerVector::create();
    std::set_difference(p_events.begin(), p_events.end(),
                        q_events.begin(), q_events.end(),
                        std::inserter(p_q_events, p_q_events.end()));
    p_q_events_list.push_back(p_q_events);
  }
  
  // Initialize paired vectors of actor neighbors of q
  std::vector<int> p_vec;
  std::vector<int> r_vec;
  std::vector<bool> cl_vec;
  // Initialize intersections and differences
  std::vector<int> pqr_events;
  std::vector<int> pr_events;
  std::vector<int> pr_q_events;
  // Add pairs with mutually exclusive events
  bool cl;
  for (i = 0; i < q_actors_count - 1; i++) {
    for (j = i + 1; j < q_actors_count; j++) {
      // Calculate the events shared by p and r
      pr_events.clear();
      std::set_intersection(p_events_list[i].begin(),
                            p_events_list[i].end(),
                            p_events_list[j].begin(),
                            p_events_list[j].end(),
                            std::back_inserter(pr_events));
      std::sort(pr_events.begin(), pr_events.end());
      // Calculate the events shared by p and r
      pqr_events.clear();
      std::set_intersection(pq_events_list[i].begin(),
                            pq_events_list[i].end(),
                            pq_events_list[j].begin(),
                            pq_events_list[j].end(),
                            std::back_inserter(pqr_events));
      // Calculate the events shared by p and r but not q
      pr_q_events.clear();
      std::set_difference(pr_events.begin(), pr_events.end(),
                          q_events.begin(), q_events.end(),
                          std::inserter(pr_q_events, pr_q_events.end()));
      // Keep a wedge with exclusive events shared with p and r
      if ((pq_events_list[i].size() > pqr_events.size()) &&
          (pq_events_list[j].size() > pqr_events.size())) {
        p_vec.push_back(q_actors[i]);
        r_vec.push_back(q_actors[j]);
        // If p and r share an event,
        // then the wedge is closed
        cl = (pr_events.size() > 0);
        cl_vec.push_back(cl);
      }
      // Keep a wedge with an exclusive event with p and an inclusive one with r
      if ((pq_events_list[i].size() > pqr_events.size()) &&
          (pqr_events.size() > 0)) {
        p_vec.push_back(q_actors[i]);
        r_vec.push_back(q_actors[j]);
        // If p and r share an exclusive event,
        // or if there is another inclusive event,
        // then the wedge is closed
        cl = ((pr_events.size() > pqr_events.size()) ||
          (pqr_events.size() > 1));
        cl_vec.push_back(cl);
      }
      // Keep a wedge with an inclusive event with p and an exclusive one with r
      if ((pqr_events.size() > 0) &&
          (pq_events_list[j].size() > pqr_events.size())) {
        p_vec.push_back(q_actors[i]);
        r_vec.push_back(q_actors[j]);
        // If p and r share an exclusive event,
        // or if there is another inclusive event,
        // then the wedge is closed
        cl = ((pr_events.size() > pqr_events.size()) ||
          (pqr_events.size() > 1));
        cl_vec.push_back(cl);
      }
      // Keep a wedge with inclusive events shared with p and r
      if (pqr_events.size() > 1) {
        p_vec.push_back(q_actors[i]);
        r_vec.push_back(q_actors[j]);
        // If there is a third inclusive event,
        // or if p and r share an exclusive event,
        // then the wedge is closed
        cl = ((pqr_events.size() > 2) || (pr_q_events.size() > 0));
        cl_vec.push_back(cl);
      }
    }
  }
  
  // Return integer matrix and closedness indicators
  IntegerMatrix wedges(3, p_vec.size());
  LogicalVector closed(p_vec.size());
  for (i = 0; i < p_vec.size(); i++) {
    wedges(0, i) = p_vec[i];
    wedges(1, i) = q;
    wedges(2, i) = r_vec[i];
    closed(i) = cl_vec[i];
  }
  return List::create(
    _["wedges"] = wedges,
    _["closed"] = closed
  );
}

// X = T_111,0; W = T_110,0
// injective graph maps, modulo all event images
// pairs p,r of actor neighbors of q
// with distinct events shared with q
// and whether they share an additional event
// [[Rcpp::export]]
List wedges_x0w0m1c2(IntegerMatrix el, int q) {
  
  // Loop indices
  int i,j;
  
  // Incident events
  IntegerVector q_actors, q_events, p_events, pq_events;
  // Compute event (distance 1) and actor (distance 2) neighborhoods about q
  List q_ego = actor_nbhd_2(el, q);
  q_events = q_ego["d1"];
  std::sort(q_events.begin(), q_events.end());
  q_actors = q_ego["d2"];
  std::sort(q_actors.begin(), q_actors.end());
  int q_actors_count = q_actors.size();
  
  // Compute event neighborhoods about actor neigbhors of q
  std::vector<IntegerVector> p_events_list;
  std::vector<IntegerVector> pq_events_list;
  for (i = 0; i < q_actors_count; i++) {
    p_events = actor_nbhd_1(el, q_actors[i])["d1"];
    std::sort(p_events.begin(), p_events.end());
    p_events_list.push_back(p_events);
    // Restrict to events shared with q
    pq_events = IntegerVector::create();
    std::set_intersection(q_events.begin(), q_events.end(),
                          p_events.begin(), p_events.end(),
                          std::back_inserter(pq_events));
    pq_events_list.push_back(pq_events);
  }
  
  // Initialize paired vectors of actor neighbors of q
  std::vector<int> p_vec;
  std::vector<int> r_vec;
  std::vector<bool> cl_vec;
  // Initialize intersection
  std::vector<int> pr_events;
  std::vector<int> pqr_events;
  std::vector<int> pr_q_events;
  // Add all pairs
  bool pr,cl;
  for (i = 0; i < q_actors_count - 1; i++) {
    for (j = i + 1; j < q_actors_count; j++) {
      // Calculate the events shared by p and r
      pr_events.clear();
      std::set_intersection(p_events_list[i].begin(),
                            p_events_list[i].end(),
                            p_events_list[j].begin(),
                            p_events_list[j].end(),
                            std::back_inserter(pr_events));
      std::sort(pr_events.begin(), pr_events.end());
      // Calculate the events shared by p and r
      pqr_events.clear();
      std::set_intersection(pq_events_list[i].begin(),
                            pq_events_list[i].end(),
                            pq_events_list[j].begin(),
                            pq_events_list[j].end(),
                            std::back_inserter(pqr_events));
      // Calculate the events shared by p and r but not q
      pr_q_events.clear();
      std::set_difference(pr_events.begin(), pr_events.end(),
                          q_events.begin(), q_events.end(),
                          std::inserter(pr_q_events, pr_q_events.end()));
      pr = false;
      cl = false;
      // Keep a wedge with exclusive events shared with p and r
      if ((pq_events_list[i].size() > pqr_events.size()) &&
          (pq_events_list[j].size() > pqr_events.size())) {
        if (!pr) {
          p_vec.push_back(q_actors[i]);
          r_vec.push_back(q_actors[j]);
        }
        pr = true;
        // If p and r share an event,
        // then the wedge is closed
        cl = (cl || (pr_events.size() > 0));
      }
      // Keep a wedge with an exclusive event with p and an inclusive one with r
      if ((pq_events_list[i].size() > pqr_events.size()) &&
          (pqr_events.size() > 0)) {
        if (!pr) {
          p_vec.push_back(q_actors[i]);
          r_vec.push_back(q_actors[j]);
        }
        pr = true;
        // If p and r share an exclusive event,
        // or if there is another inclusive event,
        // then the wedge is closed
        cl = (cl ||
          ((pr_events.size() > pqr_events.size()) ||
          (pqr_events.size() > 1)));
      }
      // Keep a wedge with an inclusive event with p and an exclusive one with r
      if ((pqr_events.size() > 0) &&
          (pq_events_list[j].size() > pqr_events.size())) {
        if (!pr) {
          p_vec.push_back(q_actors[i]);
          r_vec.push_back(q_actors[j]);
        }
        pr = true;
        // If p and r share an exclusive event,
        // or if there is another inclusive event,
        // then the wedge is closed
        cl = (cl ||
          ((pr_events.size() > pqr_events.size()) ||
          (pqr_events.size() > 1)));
      }
      // Keep a wedge with inclusive events shared with p and r
      if (pqr_events.size() > 1) {
        if (!pr) {
          p_vec.push_back(q_actors[i]);
          r_vec.push_back(q_actors[j]);
        }
        pr = true;
        // If there is a third inclusive event,
        // or if p and r share an exclusive event,
        // then the wedge is closed
        cl = (cl ||
          ((pqr_events.size() > 2) || (pr_q_events.size() > 0)));
      }
      if (pr) {
        cl_vec.push_back(cl);
      }
    }
  }
  
  // Return integer matrix and closedness indicators
  IntegerMatrix wedges(3, p_vec.size());
  LogicalVector closed(p_vec.size());
  for (i = 0; i < p_vec.size(); i++) {
    wedges(0, i) = p_vec[i];
    wedges(1, i) = q;
    wedges(2, i) = r_vec[i];
    closed(i) = cl_vec[i];
  }
  return List::create(
    _["wedges"] = wedges,
    _["closed"] = closed
  );
}

// X = T_111,0; W = T_110,0
// induced graph maps, modulo equal event images
// (Liebig-Rao unconnected wedges and closure)
// chordless 4-paths p,a,q,b,r (with p,q,r and a,b distinct)
// and whether they are contained in chordless 6-cycles (with a,b,c distinct)
// [[Rcpp::export]]
List wedges_x0w0m2c0(IntegerMatrix el, int q) {
  
  // Loop indices
  int i,j,k,l;
  
  // Incident events
  IntegerVector q_events, a_actors, q_self, a_actors_q;
  // Compute event (distance 1) neigborhoods about q
  List q_ego = actor_nbhd_2(el, q);
  q_events = q_ego["d1"];
  std::sort(q_events.begin(), q_events.end());
  int q_events_count = q_events.size();
  // Compute actor (distance 1) neighborhoods about events of q, excluding q
  q_self = q;
  std::vector<IntegerVector> a_actors_q_list;
  for (i = 0; i < q_events_count; i++) {
    a_actors = event_nbhd_1(el, q_events[i])["d1"];
    a_actors_q = IntegerVector::create();
    std::set_difference(a_actors.begin(), a_actors.end(),
                        q_self.begin(), q_self.end(),
                        std::inserter(a_actors_q, a_actors_q.end()));
    std::sort(a_actors_q.begin(), a_actors_q.end());
    a_actors_q_list.push_back(a_actors_q);
  }
  
  // Initialize paired vectors of event and actor neighbors of q
  std::vector<int> p_vec;
  std::vector<int> a_vec;
  std::vector<int> b_vec;
  std::vector<int> r_vec;
  std::vector<bool> cl_vec;
  // Initialize intersections and differences
  std::vector<int> a_b_actors;
  std::vector<int> b_a_actors;
  std::vector<int> p_events;
  std::vector<int> r_events;
  std::vector<int> pr_events;
  std::vector<int> pr_q_events;
  // Add 4-paths (with distinct nodes)
  bool cl;
  for (i = 0; i < q_events_count - 1; i++) {
    for (j = i + 1; j < q_events_count; j++) {
      // Calculate the set differences of the actor neighborhoods of a and b
      a_b_actors.clear();
      std::set_difference(a_actors_q_list[i].begin(),
                          a_actors_q_list[i].end(),
                          a_actors_q_list[j].begin(),
                          a_actors_q_list[j].end(),
                          std::inserter(a_b_actors, a_b_actors.end()));
      b_a_actors.clear();
      std::set_difference(a_actors_q_list[j].begin(),
                          a_actors_q_list[j].end(),
                          a_actors_q_list[i].begin(),
                          a_actors_q_list[i].end(),
                          std::inserter(b_a_actors, b_a_actors.end()));
      for (k = 0; k < a_b_actors.size(); k++) {
        for (l = 0; l < b_a_actors.size(); l++) {
          // Keep the 4-path as a wedge
          p_vec.push_back(a_b_actors[k]);
          a_vec.push_back(q_events[i]);
          b_vec.push_back(q_events[j]);
          r_vec.push_back(b_a_actors[l]);
          // If p and r share an event not shared by q,
          // then the wedge is closed
          // Calculate the event (distance 1) neighborhoods of p and r
          p_events = actor_nbhd_1(el, a_b_actors[k])["d1"];
          r_events = actor_nbhd_1(el, b_a_actors[l])["d1"];
          // Calculate the intersection of the events of p and of r
          pr_events.clear();
          std::set_intersection(p_events.begin(),
                                p_events.end(),
                                r_events.begin(),
                                r_events.end(),
                                std::back_inserter(pr_events));
          std::sort(pr_events.begin(), pr_events.end());
          // Calculate the set difference of the shared events of p, r, and of q
          pr_q_events.clear();
          std::set_difference(pr_events.begin(),
                              pr_events.end(),
                              q_events.begin(),
                              q_events.end(),
                              std::inserter(pr_q_events, pr_q_events.end()));
          cl = (pr_q_events.size() > 0);
          cl_vec.push_back(cl);
        }
      }
    }
  }
  
  // Return integer matrix and closedness indicators
  IntegerMatrix wedges(5, p_vec.size());
  LogicalVector closed(p_vec.size());
  for (i = 0; i < p_vec.size(); i++) {
    wedges(0, i) = p_vec[i];
    wedges(1, i) = a_vec[i];
    wedges(2, i) = q;
    wedges(3, i) = b_vec[i];
    wedges(4, i) = r_vec[i];
    closed(i) = cl_vec[i];
  }
  return List::create(
    _["wedges"] = wedges,
    _["closed"] = closed
  );
}

// X = T_111,0; W = T_110,0
// induced graph maps, modulo structurally equivalent event images
// induced graph maps, modulo all event images
// (exclusive wedges and closure)
// pairs p,r of actor neighbors of q
// with mutually exclusive events shared with q
// and whether they share an event exclusive from q
// [[Rcpp::export]]
List wedges_x0w0m2c1(IntegerMatrix el, int q) {
  
  // Loop indices
  int i,j;
  
  // Incident events
  IntegerVector q_actors, q_events, p_events, pq_events;
  // Compute event (distance 1) and actor (distance 2) neighborhoods about q
  List q_ego = actor_nbhd_2(el, q);
  q_events = q_ego["d1"];
  std::sort(q_events.begin(), q_events.end());
  q_actors = q_ego["d2"];
  std::sort(q_actors.begin(), q_actors.end());
  int q_actors_count = q_actors.size();
  
  // Compute event neighborhoods about actor neigbhors of q
  std::vector<IntegerVector> p_events_list;
  std::vector<IntegerVector> pq_events_list;
  for (i = 0; i < q_actors_count; i++) {
    p_events = actor_nbhd_1(el, q_actors[i])["d1"];
    std::sort(p_events.begin(), p_events.end());
    p_events_list.push_back(p_events);
    // Restrict to events shared with q
    pq_events = IntegerVector::create();
    std::set_intersection(q_events.begin(), q_events.end(),
                          p_events.begin(), p_events.end(),
                          std::back_inserter(pq_events));
    pq_events_list.push_back(pq_events);
  }
  
  // Initialize paired vectors of actor neighbors of q
  std::vector<int> p_vec;
  std::vector<int> r_vec;
  std::vector<bool> cl_vec;
  // Initialize intersections and differences
  std::vector<int> pqr_events;
  std::vector<int> pr_events;
  std::vector<int> pr_q_events;
  // Add pairs with mutually exclusive events
  bool cl;
  for (i = 0; i < q_actors_count - 1; i++) {
    for (j = i + 1; j < q_actors_count; j++) {
      // If the intersection is smaller than the event neighborhoods of p and r,
      // then keep the pair q_actors[i], q_actors[j]
      pqr_events.clear();
      std::set_intersection(pq_events_list[i].begin(),
                            pq_events_list[i].end(),
                            pq_events_list[j].begin(),
                            pq_events_list[j].end(),
                            std::back_inserter(pqr_events));
      if ((pq_events_list[i].size() > pqr_events.size()) &&
          (pq_events_list[j].size() > pqr_events.size())) {
        p_vec.push_back(q_actors[i]);
        r_vec.push_back(q_actors[j]);
        // If intersection of p & r events is larger than also with q_events,
        // then the wedge is closed
        pr_events.clear();
        std::set_intersection(p_events_list[i].begin(),
                              p_events_list[i].end(),
                              p_events_list[j].begin(),
                              p_events_list[j].end(),
                              std::back_inserter(pr_events));
        std::sort(pr_events.begin(), pr_events.end());
        pr_q_events.clear();
        std::set_difference(pr_events.begin(), pr_events.end(),
                            q_events.begin(), q_events.end(),
                            std::inserter(pr_q_events, pr_q_events.end()));
        cl = (pr_q_events.size() > 0);
        cl_vec.push_back(cl);
      }
    }
  }
  
  // Return integer matrix and closedness indicators
  IntegerMatrix wedges(3, p_vec.size());
  LogicalVector closed(p_vec.size());
  for (i = 0; i < p_vec.size(); i++) {
    wedges(0, i) = p_vec[i];
    wedges(1, i) = q;
    wedges(2, i) = r_vec[i];
    closed(i) = cl_vec[i];
  }
  return List::create(
    _["wedges"] = wedges,
    _["closed"] = closed
  );
}

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

// Triad census for affiliation networks

// algorithm adapted from Batagelj and Mrvar (2001)
// performed on an edgelist
// [[Rcpp::export]]
IntegerMatrix triad_census_batagelj_mrvar_C(
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
