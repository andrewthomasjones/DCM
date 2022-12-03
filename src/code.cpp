#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

//' @importFrom Rcpp sourceCpp
//' @useDynLib DCM

//' @export
// [[Rcpp::export]]
arma::mat frequencyDistributionCpp(const arma::mat& cold) {
  // init
  int i3 = 1;
  int i4, temp1, temp2;
  int s1_r = cold.n_rows;
  int s1_c = cold.n_cols;
  bool not_found = false;

  arma::mat  fd = arma::zeros<arma::mat>(s1_r,2);
  fd(0,0) = cold(0,0);
  fd(0,1) = 1.0;

  for (int i1 = 1; i1 < s1_r; i1++){
    not_found = true;
    for(int i2 = 0; i2 < i3; i2++){
      if(cold(i1,0) == fd(i2,0)){
        fd(i2,1) = fd(i2,1) + 1.0;
        not_found = false;
      }
    }
    if(not_found == true){
      i3++;
      fd(i3,0) = cold(i1,0);
      fd(i3,1) = 1;
    }
    Rcout << "i3: " << i3 << '\n';
  }
  Rcout << fd.n_rows << '\n';
  Rcout << fd.n_cols << '\n';
  double x = accu(fd);
  Rcout << x << '\n';

  arma::mat  fdd = arma::zeros<arma::mat>(i3,2);
  fdd(0,0) = fd(0,0);
  fdd(0,1) = fd(0,1);

  for (int i5 = 1; i5 < i3; i5++){
    fdd(i5,0) = fd(i5,0);
    fdd(i5,1) = fd(i5,1);
    i4 = i5;

    while(i4>1){
      //Rcout << i4 << '\n';
      if(fdd(i4,0) < fdd(i4-1,0)){
        temp1 = fdd(i4,0);
        temp2 = fdd(i4,1);
        fdd(i4,0) = fdd(i4-1,0);
        fdd(i4,1) = fdd(i4-1,1);
        fdd(i4-1,0) = temp1;
        fdd(i4-1,1) = temp2;
      }
      i4--;
    }
  }

  return(fdd);
}
