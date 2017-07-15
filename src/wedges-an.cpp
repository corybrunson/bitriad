// survey wedges and determine closure

// #include <iostream>
// #include <vector>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List actor_events(IntegerMatrix el, int q) {
  
  int m = el.nrow();
  
  // Identify the sphere of radius 1 about q
  std::vector<int> n1;
  int i;
  for (i = 0; i < m; i++) {
    if (el(i, 0) == q) {
      if(std::find(n1.begin(), n1.end(), el(i, 1)) != n1.end()) {
        continue;
      } else {
        n1.push_back(el(i, 1));
      }
    }
  }
  
  return List::create(Named("d0") = q,
                      Named("d1") = n1);
}

// [[Rcpp::export]]
List event_actors(IntegerMatrix el, int a) {
  
  int m = el.nrow();
  
  // Identify the sphere of radius 1 about a
  std::vector<int> n1;
  int i;
  for (i = 0; i < m; i++) {
    if (el(i, 1) == a) {
      if(std::find(n1.begin(), n1.end(), el(i, 0)) != n1.end()) {
        continue;
      } else {
        n1.push_back(el(i, 0));
      }
    }
  }
  
  return List::create(Named("d0") = a,
                      Named("d1") = n1);
}

// [[Rcpp::export]]
List actor_actors(IntegerMatrix el, int q) {
  
  int m = el.nrow();
  
  // Identify the spheres of radius 1 and 2 about q
  std::vector<int> n1;
  std::vector<int> n2;
  int i, j;
  for (i = 0; i < m; i++) {
    if (el(i, 0) == q) {
      if(std::find(n1.begin(), n1.end(), el(i, 1)) != n1.end()) {
        continue;
      } else {
        n1.push_back(el(i, 1));
        for (j = 0; j < m; j++) {
          if (j == i) {
            continue;
          }
          if (el(j, 1) == el(i, 1)) {
            if(std::find(n2.begin(), n2.end(), el(j, 0)) != n2.end()) {
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
//   0 <-> same actor images
//   1 <-> same actor images, structurally equivalent event images
//   2 <-> same actor images, same event images

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
  List q_ego = actor_actors(el, q);
  q_events = q_ego["d1"];
  std::sort(q_events.begin(), q_events.end());
  int q_events_count = q_events.size();
  // Compute actor (distance 1) neighborhoods about events of q, excluding q
  q_self = q;
  std::vector<IntegerVector> a_actors_q_list;
  for (i = 0; i < q_events_count; i++) {
    a_actors = event_actors(el, q_events[i])["d1"];
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
          p_events = actor_events(el, a_actors_q_list[i][k])["d1"];
          r_events = actor_events(el, a_actors_q_list[j][l])["d1"];
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
  List q_ego = actor_actors(el, q);
  q_events = q_ego["d1"];
  std::sort(q_events.begin(), q_events.end());
  q_actors = q_ego["d2"];
  std::sort(q_actors.begin(), q_actors.end());
  int q_actors_count = q_actors.size();
  
  // Compute event neighborhoods about actor neigbhors of q
  std::vector<IntegerVector> p_events_list;
  std::vector<IntegerVector> pq_events_list;
  for (i = 0; i < q_actors_count; i++) {
    p_events = actor_events(el, q_actors[i])["d1"];
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
  List q_ego = actor_actors(el, q);
  q_events = q_ego["d1"];
  std::sort(q_events.begin(), q_events.end());
  q_actors = q_ego["d2"];
  std::sort(q_actors.begin(), q_actors.end());
  int q_actors_count = q_actors.size();
  
  // Compute event neighborhoods about actor neigbhors of q
  std::vector<IntegerVector> p_events_list;
  for (i = 0; i < q_actors_count; i++) {
    p_events = actor_events(el, q_actors[i])["d1"];
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
  List q_ego = actor_actors(el, q);
  q_events = q_ego["d1"];
  std::sort(q_events.begin(), q_events.end());
  int q_events_count = q_events.size();
  // Compute actor (distance 1) neighborhoods about events of q, excluding q
  q_self = q;
  std::vector<IntegerVector> a_actors_q_list;
  for (i = 0; i < q_events_count; i++) {
    a_actors = event_actors(el, q_events[i])["d1"];
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
          p_events = actor_events(el, a_actors_q_list[i][k])["d1"];
          r_events = actor_events(el, a_actors_q_list[j][l])["d1"];
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
  List q_ego = actor_actors(el, q);
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
    p_events = actor_events(el, q_actors[i])["d1"];
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
  List q_ego = actor_actors(el, q);
  q_events = q_ego["d1"];
  std::sort(q_events.begin(), q_events.end());
  q_actors = q_ego["d2"];
  std::sort(q_actors.begin(), q_actors.end());
  int q_actors_count = q_actors.size();
  
  // Compute event neighborhoods about actor neigbhors of q
  std::vector<IntegerVector> p_events_list;
  std::vector<IntegerVector> pq_events_list;
  for (i = 0; i < q_actors_count; i++) {
    p_events = actor_events(el, q_actors[i])["d1"];
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
// chordless 4-paths p,a,q,b,r (with p,q,r and a,b distinct)
// and whether they are contained in chordless 6-cycles (with a,b,c distinct)
// [[Rcpp::export]]
List wedges_x0w0m2c0(IntegerMatrix el, int q) {
  
  // Loop indices
  int i,j,k,l;
  
  // Incident events
  IntegerVector q_events, a_actors, q_self, a_actors_q;
  // Compute event (distance 1) neigborhoods about q
  List q_ego = actor_actors(el, q);
  q_events = q_ego["d1"];
  std::sort(q_events.begin(), q_events.end());
  int q_events_count = q_events.size();
  // Compute actor (distance 1) neighborhoods about events of q, excluding q
  q_self = q;
  std::vector<IntegerVector> a_actors_q_list;
  for (i = 0; i < q_events_count; i++) {
    a_actors = event_actors(el, q_events[i])["d1"];
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
          p_events = actor_events(el, a_b_actors[k])["d1"];
          r_events = actor_events(el, b_a_actors[l])["d1"];
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
  List q_ego = actor_actors(el, q);
  q_events = q_ego["d1"];
  std::sort(q_events.begin(), q_events.end());
  q_actors = q_ego["d2"];
  std::sort(q_actors.begin(), q_actors.end());
  int q_actors_count = q_actors.size();
  
  // Compute event neighborhoods about actor neigbhors of q
  std::vector<IntegerVector> p_events_list;
  std::vector<IntegerVector> pq_events_list;
  for (i = 0; i < q_actors_count; i++) {
    p_events = actor_events(el, q_actors[i])["d1"];
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
