#include <Rcpp.h>

typedef std::map<unsigned, unsigned> nbhd_level_map;
typedef std::vector<int> int_vec_jc; //To avoid conflicts,
//I typically append _(initials) to any classes/structs/typedefs
//
typedef std::vector<int_vec_jc> int_vec_2d_jc;

//THESE ARE ALL THINGS THAT SHOULD NOT BE CALLED IN CENSUSES
int_vec_jc valuecount(nbhd_level_map m){
  nbhd_level_map::iterator it;
  unsigned max =0;
  for(it=m.begin();it!=m.end();it++){
    if(it->second > max)
      max = it->second;
  }
  int_vec_jc toRet(max+1,0);
  for(it=m.begin();it!=m.end();it++)
    toRet[it->second]++;
  return toRet;
}

int_vec_2d_jc invert_map(nbhd_level_map m){
  int_vec_2d_jc toRet;
  //This is optional, but is useful for larger nbhds
  int_vec_jc n_values = valuecount(m);
  for(int i=0;i<n_values.size();++i)
    toRet[i].reserve(n_values[i]);
  
  
  for(nbhd_level_map::iterator it=m.begin();
    it!=m.end();it++)
    toRet[it->second].push_back(it->first);
  return toRet;
}



std::set<int> get_neighbors_for(int ci,IntegerMatrix el, std::set<int> s){
  std::set<int> result;
  int targetIndex = 1-ci;
  for(int i=0;i<el.nrow();i++) //set find gives you a bit of a performance 
    //boost in this case since it's
    //logarithmic search
    if (s.find(el(i,ci)) != s.end())
      result.insert(el(i,targetIndex));
  return result;
}

void add_set_to_map(nbhd_level_map & m,std::set<int> s, int val){
  std::set<int>::iterator it;
  for(it=s.begin();it!=s.end();++it)
    m.insert(std::make_pair(*it, val));
}


nbhd_level_map nbhd_x(const int levels,int ci, IntegerMatrix el,int q){
  nbhd_level_map toRet;
  std::set<int> res;
  res.insert(q);
  add_set_to_map(toRet, res, 0);
  for(int i=1;i<=levels;i++){
    res = get_neighbors_for(ci, el,res);
    add_set_to_map(toRet, res, i);
    ci = (ci == 0) ? 1 : 0;
  }
  return toRet;
}

//END THINGS YOU SHOULD NOT CALL IN CENSUSES
//USE THESE TO ACCESS NEIGHBORHOODS
int_vec_2d_jc actor_nbhd(const int levels,IntegerMatrix el,int q){
  return invert_map(nbhd_x(levels, 0,el,q));
}

int_vec_2d_jc event_nbhd(const int levels,IntegerMatrix el,int q){
  return invert_map(nbhd_x(levels, 1,el,q));
}