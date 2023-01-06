#include "RcppArmadillo.h"

// [[Rcpp::depends(RcppArmadillo)]]

//' @importFrom Rcpp sourceCpp
//' @useDynLib DCM

using namespace Rcpp;

//' @export
// [[Rcpp::export]]
arma::mat frequencyDistributionCpp(const arma::mat& cold) {
  // init
  int i3 = 0;
  int i4, temp1, temp2;

  int s1_r = cold.n_rows;
  int s1_c = cold.n_cols;
  bool not_found = false;

  arma::mat  fd = arma::zeros<arma::mat>(s1_r,2);
  fd(0,0) = cold(0,0);
  fd(0,1) = 1;

  for (int i1 = 1; i1 < s1_r; i1++){
    not_found = true;
    for(int i2 = 0; i2 < (i3+1); i2++){
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
  }
  double x = accu(fd);

  arma::mat  fdd = arma::zeros<arma::mat>(i3+1,2);
  fdd(0,0) = fd(0,0);
  fdd(0,1) = fd(0,1);

  for (int i5 = 1; i5 < (i3+1); i5++){
    fdd(i5,0) = fd(i5,0);
    fdd(i5,1) = fd(i5,1);
    i4 = i5;

    while(i4>1){
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


//' @export
// [[Rcpp::export]]
Rcpp::List createConceptsCpp(const arma::mat& data_matrix, int nmax_choiceset_size = 31) {

  int nconcepts, nlines_data, ncovariates, nlines_data_matrix, match_number, match_count;
  int i1, i2, i3, i4, i5, i6;
  int iddm, idcs;

  nlines_data_matrix = data_matrix.n_rows;
  ncovariates = data_matrix.n_cols - 3;

  iddm = data_matrix(0, 0);
  idcs = data_matrix(0, 1);

  arma::mat data_big(nlines_data_matrix, nmax_choiceset_size + 4, arma::fill::zeros);
  data_big(0, 0) = iddm;

  if (data_matrix(0, 2) == 1){
    data_big(0, 1) = 1.0;
  }
  data_big(0, 3) = 1.0;

  arma::mat concept_big(nlines_data_matrix,ncovariates, arma::fill::zeros);

  for (int i3=0; i3<ncovariates; i3++){
    concept_big(0, i3) = data_matrix(0, i3 + 3);
  }

  data_big(0, 4) = 1.0;

  // i1 is the count of lines moving through the data matrix
  // i2 is the build of the number of rows in data_big
  // i3 is the count of covariates moving through the row of the data matrix
  // i4 is the count of lines moving through the concept matrix
  // i5 is the build of the number of concepts in a row in data_big
  // i6 is build of the number of concepts

  i1 = 1;
  i2 = 1;
  i5 = 1;
  i6 = 1;

  while (i1 < nlines_data_matrix) {
    i1++;
    match_number=0;
    i4=0;

    while (i4 < i6) {
      i4++;
      match_count=0;

      for (int i3=0; i3<ncovariates; i3++) {
        if (concept_big(i4, i3) == data_matrix(i1, i3 + 3)){
          match_count++;
        }
      }

      if (match_count == ncovariates) {
        match_number = i4;
        i4 = i6;
      }
    }

    if (match_number == 0) {
      i6++;
      match_number = i6;

      for (int i3=0; i3<ncovariates; i3++) {
        concept_big(i6, i3) = data_matrix(i1, i3 + 3);
      }


      if (data_matrix(i1, 1) == iddm) {
        if (data_matrix(i1, 2) == idcs) {
          i5++;
          data_big(i2, i5 + 4) = match_number;

          if (data_matrix(i1, 3) == 1){
            data_big(i2, 2) = match_number;
          }
        }
      }

      if (data_matrix(i1, 1) == iddm) {
        if (data_matrix(i1, 2) > idcs) {
          idcs = data_matrix(i1, 2);
          i5 = 1;
          i2++;
          data_big(i2, 1) = iddm;
          data_big(i2, 4) = 1;
          data_big(i2, i5 + 4) = match_number;

          if (data_matrix(i1, 3) == 1){
            data_big(i2, 2) = match_number;
          }
        }
      }

      if (data_matrix(i1, 1) > iddm) {
        iddm = data_matrix(i1, 1);
        idcs = data_matrix(i1, 2);
        i5=1;
        i2++;
        data_big(i2, 1) = iddm;
        data_big(i2, 4) = 1;
        data_big(i2, i5 + 4) = match_number;

        if (data_matrix(i1, 3) == 1){
          data_big(i2, 2) = match_number;
        }

      }


    }


    arma::mat concept = arma::mat(i6, ncovariates, arma::fill::zeros);

    for (int i8=0; i8<i6; i8++) {
      for (int i9=0; i9<ncovariates; i9++) {
        concept(i8, i9) = concept_big(i8, i9);
      }
    }


  }

  nconcepts = i6;

  arma::mat data = arma::mat(i2, i2, arma::fill::zeros);

  //replace with a bulk copy
  for (int i8 = 0; i8 < i2; i8++){
    for (int i9 = 0; i9 < (nmax_choiceset_size + 4); i9++){
      data(i8, i9) = data_big(i8, i9);
    }
  }

  nlines_data = i2;



  //arma::mat data = arma::mat(3, 3, arma::fill::zeros);
  //arma::mat data_big = arma::mat(5, 5, arma::fill::zeros);
  //nconcepts=10;
  //nlines_data=10;

  Rcpp::List L = Rcpp::List::create(Rcpp::Named("data") = data,
                                    Rcpp::Named("nconcepts") = nconcepts,
                                    Rcpp::Named("nlines_data") = nlines_data,
                                    Rcpp::Named("data_big") = data_big
  );

  return(L);

}











