#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector which_rcpp(StringVector corp, StringVector dict){
  int n = corp.size();
  int j = 0;
  IntegerVector res(n);
  for(int i = 0; i < n; i++ ){
    j = 0;
    while(!(corp[i] == dict[j])){
      j = j+1;
    }
    res[i] = j+1;
  }
  return res;
}