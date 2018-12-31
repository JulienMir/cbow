#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

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