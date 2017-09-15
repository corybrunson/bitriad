#include <map>
#include <vector>

// std::map object type specific to (ordered) neighborhoods
typedef std::map<int, int> nbhd_order_map_jcb;
// integer vector type for internal use
//typedef std::vector<int> int_vec_jcb;

// number of nodes in each concentric tier of the neighborhood
std::vector<int> order_counts(nbhd_order_map_jcb m) {
  nbhd_order_map_jcb::iterator it;
  int max = 0;
  for (it = m.begin(); it != m.end(); it++) {
    if (it->second > max) {
      max = it->second;
    }
  }
  std::vector<int> ret(max + 1, 0);
  for (it = m.begin(); it != m.end(); it++) {
    ret[it->second]++;
  }
  return ret;
}

// 
std::vector<std::vector<int> > invert_map(nbhd_order_map_jcb m) {
  std::vector< std::vector<int> > ret;
  // reserve space; optional but useful for large neighborhoods
  std::vector<int> order_n = order_counts(m);
  for (int i = 0; i < order_n.size(); ++i) {
    ret[i].reserve(order_n[i]);
  }
  // "unwind" map?
  for (nbhd_order_map_jcb::iterator it = m.begin(); it != m.end(); it++) {
    ret[it->second].push_back(it->first);
  }
  return ret;
}

// collect all nodes incident to the given node
// (necessarily of the other mode)
std::set<int> get_alters_for(int ae, std::vector<std::vector<int> > el, std::set<int> s) {
  std::set<int> res;
  for (int i = 0; i < el.nrow(); i++) {
    if (s.find(el(i, ae)) != s.end()) {
      res.insert(el(i, 1 - ae));
    }
  }
  return res;
}

void add_set_to_map3(nbhd_order_map_jcb & m, std::set<int> s, int val) {
  std::set<int>::iterator it;
  for (it = s.begin(); it != s.end(); ++it) {
    m.insert(std::make_pair(*it, val));
  }
}

// neighborhood of a given order centered at a given node
nbhd_order_map_jcb node_nbhd(
    const int order, int ae, std::vector<std::vector<int> > el, int q
) {
  nbhd_order_map_jcb ret;
  std::set<int> res;
  res.insert(q);
  add_set_to_map3(ret, res, 0);
  for (int i = 1; i <= order; i++) {
    res = get_alters_for(ae, el, res);
    add_set_to_map3(ret, res, i);
    ae = (ae == 0) ? 1 : 0;
  }
  return ret;
}

// [[Rcpp::export]]
std::vector<std::vector<int> > actor_nbhd(const int order, std::vector<std::vector<int> > el, int q) {
  return invert_map(node_nbhd(order, 0, el, q));
}

// [[Rcpp::export]]
std::vector<std::vector<int> >  event_nbhd(const int order,std::vector<std::vector<int> > el, int q){
  return invert_map(node_nbhd(order, 1, el, q));
}

int main() {return 0;}
