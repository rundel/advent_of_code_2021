#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int sim(std::vector<int> x, int days) {
  
  for(int i=0; i<days; i++) {
    Rcpp::Rcout << i << "\n"; 
    
    int n = 0;
    for(int j=0; j < x.size(); j++ ) {
      x[j]--;
      if (x[j] == -1) {
        n++;
        x[j] = 6;
      }
    }
    
    for(int j=0; j < n; j++) {
      x.push_back(8);
    }
    
    //for(int j=0; j < x.size(); j++ ) {
    //  Rcpp::Rcout << x[j] << " ";
    //}
    Rcpp::Rcout << "\n";
  }
  
  return(x.size());
}
