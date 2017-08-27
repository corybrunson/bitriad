#include <Rcpp.h>

using namespace Rcpp;

IntegerVector mode_nbhd(std::vector<IntegerVector> al, int q) {
  
  int a,p;
  IntegerVector nbhd;
  
  // Recall that igraph node indices begin at 1, so node indices must be
  // decreased by 1 to be used as positions.
  for (int i = 0; i < al[q - 1].size(); i++) {
    a = al[q - 1][i];
    for (int j = 0; j < al[a - 1].size(); j++) {
      p = al[a - 1][j];
      if ((p == q) |
          (std::find(nbhd.begin(), nbhd.end(), p) != nbhd.end())) {
        continue;
      } else {
        nbhd.push_back(p);
        std::sort(nbhd.begin(), nbhd.end());
      }
    }
  }
  
  return nbhd;
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
//   0 <-> same actor images, same event images
//   1 <-> same actor images, structurally equivalent event images
//   2 <-> same actor images

// Wedge censuses and closure indicators

// X = T_111,0; W = T_110,0
// all graph maps, modulo equal event images
// 4-paths p,a,q,b,r (with p,q,r distinct)
// and whether they are contained in 6-cycles
// [[Rcpp::export]]
List wedges_adjlist_x0w0m0c0(std::vector<IntegerVector> al, int q) {
  
  // Loop indices
  int i,j,k,l;
  // Incident events
  IntegerVector q_events, a_actors, b_actors, p_events, r_events;
  
  // Initialize paired vectors of event and actor neighbors of q
  std::vector<int> p_vec;
  std::vector<int> a_vec;
  std::vector<int> b_vec;
  std::vector<int> r_vec;
  std::vector<bool> cl_vec;
  // Initialize intersections and differences
  std::vector<int> pr_events;
  bool cl;
  
  // Compute event (distance 1) neigborhoods about q
  q_events = al[q - 1];
  // Add 4-paths (with distinct actors)
  for (i = 0; i < q_events.size(); i++) {
    a_actors = al[q_events[i] - 1];
    for (j = i; j < q_events.size(); j++) {
      b_actors = al[q_events[j] - 1];
      for (k = 0; k < a_actors.size(); k++) {
        if (a_actors[k] == q) {
          continue;
        }
        for (l = 0; l < b_actors.size(); l++) {
          // Ensure that actors are distinct
          if ((b_actors[l] == q) | (b_actors[l] == a_actors[k])) {
            continue;
          }
          // If the events are equal, then ensure that the actors are ordered
          if ((q_events[i] == q_events[j]) && (a_actors[k] > b_actors[l])) {
            continue;
          }
          // Calculate the event (distance 1) neighborhoods of p and r
          p_events = al[a_actors[k] - 1];
          r_events = al[b_actors[l] - 1];
          // Calculate the intersection of the events of p and of r
          pr_events.clear();
          std::set_intersection(p_events.begin(),
                                p_events.end(),
                                r_events.begin(),
                                r_events.end(),
                                std::back_inserter(pr_events));
          // Keep the 4-path as a wedge
          p_vec.push_back(a_actors[k]);
          a_vec.push_back(q_events[i]);
          b_vec.push_back(q_events[j]);
          r_vec.push_back(b_actors[l]);
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
