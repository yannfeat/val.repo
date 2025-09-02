#include <Rcpp.h>
using namespace Rcpp;

//' Add slices and return vector of the sums of slices
//'
//' @param slicematrix A matrix of slices.
//' @return Returns the vector of the sums of the slices
//' @export
// [[Rcpp::export]]
NumericVector AddSlices_Rcpp(NumericMatrix slicematrix){
    double n = slicematrix.nrow()-1;
    NumericVector x (n+1);
    double tmp = 0;

    for(int k=0; k<= n; ++k){

      tmp = 0;

      for(int j=1; j<= k+1; ++j){ // moved indices by 1


        for(int i=k+2-j; i <= n+2-j; ++i){
          tmp =tmp+ slicematrix(i-1,j-1); // moved indices by 1
        }

      }
      x[k] = tmp;// moved index by 1

    }
    return x;
  }

//' Add slices and return vector of the weighted sums of slices
//'
//' @param slicematrix A matrix of slices.
//' @param weightvector A vector of weights.
//' @return Returns the vector of the weighted sums of the slices
//' @export
// [[Rcpp::export]]
NumericVector AddWeightedSlices_Rcpp(NumericMatrix slicematrix, NumericVector weightvector){
  double n = slicematrix.nrow()-1;
  NumericVector x (n+1);
  double tmp = 0;

  for(int k=0; k<= n; ++k){

    tmp = 0;

    for(int j=1; j<= k+1; ++j){ // moved indices by 1


      for(int i=k+2-j; i <= n+2-j; ++i){
        tmp =tmp+ weightvector[k+2-j-1]*slicematrix(i-1,j-1); // moved indices by 1
      }

    }
    x[k] = tmp;// moved index by 1

  }
  return x;
}




