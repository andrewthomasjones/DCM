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

  int ncs4 = nmax_choiceset_size + 4;

  nlines_data_matrix = data_matrix.n_rows;
  ncovariates = data_matrix.n_cols - 3;

  iddm = data_matrix(0, 0);
  idcs = data_matrix(0, 1);

  arma::mat data_big = arma::zeros<arma::mat>(nlines_data_matrix, ncs4);
 //Rout << "point 1" <<  std::endl;
  data_big(0, 0) = iddm;

  if (data_matrix(0, 2) == 1){
    data_big(0, 1) = 1;
  }
  data_big(0, 3) = 1;

  arma::mat concept_big(nlines_data_matrix, ncovariates, arma::fill::zeros);

  concept_big.row(0) = data_matrix(0, arma::span(3, 3+ncovariates-1));

  data_big(0, 4) = 1;

  i1 = 0;
  i2 = 0;
  i5 = 0;
  i6 = 0;
  i4 = -1;
 //Rout << "point 2" <<  std::endl;
  //Rcout << "data " << data.n_rows << " " << data.n_cols <<  std::endl;
  //Rcout << "data_big " << data_big.n_rows << " " << data_big.n_cols <<  std::endl;
  //Rcout << "concept_big " << concept_big.n_rows << " " << concept_big.n_cols <<  std::endl;
  //Rcout << "data_matrix " << data_matrix.n_rows << " " << data_matrix.n_cols <<  std::endl;

  while (i1 < (nlines_data_matrix-1)){
    // if(i1<10){
    //   Rcout << "i1 " << i1 << " i2 " << i2 << " i4 " << i4 << " i5 " << i5 << " i6 " << i6 << std::endl;
    // }

    i1++;
    match_number=0;
    i4=-1;

    while (i4 < i6) {
      i4++;
      match_count=0;

      for (int i3=0; i3<ncovariates; i3++) {
        if (concept_big(i4, i3) == data_matrix(i1, i3 + 3)){
          match_count++;
        }
      }

      if (match_count == ncovariates) {
        match_number = i4 +1;
        i4 = i6;
      }
    }
    //Rcout << "Point 3 " << i1 <<  std::endl;
    // if(i1<10){
    //   Rcout << " i4 " << i4 << " i6 " << i6 << std::endl;
    // }

    if (match_number == 0) {
      i6++;
      match_number = i6 +1;
      concept_big.row(i6) = data_matrix(i1, arma::span(3, 3+ncovariates-1));
    }

    //Rcout << "Point 4 " << i1 <<  std::endl;

    if (data_matrix(i1, 0) == iddm) {
      if (data_matrix(i1, 1) == idcs) {
        i5++;
       //Rout << "Point 4.1 i1 " << i1 << " i5 "<< i5 << " i2 "<< i2 <<  " data big size: "<< data_big.n_rows << " " << data_big.n_cols << std::endl;
        data_big(i2, i5 + 4) = match_number;
       //Rout << "Point 4.2 i1 " << i1 << " i5 "<< i5 << std::endl;
        if (data_matrix(i1, 2) == 1){
          data_big(i2, 1) = match_number;
         //Rout << "Point 4.3 " << i1 <<  std::endl;
        }
      }
    }

   //Rout << "Point 5 " << i1 <<  std::endl;

    if (data_matrix(i1, 0) == iddm) {
      if (data_matrix(i1, 1) > idcs) {
        idcs = data_matrix(i1, 1);
        i5 = 0;
        i2++;
        data_big(i2, 0) = iddm;
        data_big(i2, 3) = 1;
        data_big(i2, i5 + 4) = match_number;

        if (data_matrix(i1, 2) == 1){
          data_big(i2, 1) = match_number;
        }
      }
    }
   //Rout << "Point 6 " << i1 <<  std::endl;
    if (data_matrix(i1, 0) > iddm) {
      iddm = data_matrix(i1, 0);
      idcs = data_matrix(i1, 1);
      i5=0;
      i2++;
      data_big(i2, 0) = iddm;
      data_big(i2, 3) = 1;
      data_big(i2, i5 + 4) = match_number;

      if (data_matrix(i1, 2) == 1){
        data_big(i2, 1) = match_number;
      }

    }

  }

 //Rout << "final point" <<  std::endl;

  nconcepts = i6+1;
  nlines_data = i2+1;

  arma::mat concept = arma::mat(i6+1, ncovariates, arma::fill::zeros);
  concept(arma::span::all, arma::span::all) =
    concept_big(arma::span(0,i6), arma::span(0,ncovariates-1));

  arma::mat data = arma::zeros<arma::mat>(i2+1, (nmax_choiceset_size + 4));
  data(arma::span::all, arma::span::all) =
    data_big(arma::span(0,i2), arma::span(0,ncs4-1));

 //Rout << "make list" <<  std::endl;

  Rcpp::List L = Rcpp::List::create(Rcpp::Named("data") = data,
                                    Rcpp::Named("nconcepts") = nconcepts,
                                    Rcpp::Named("nlines_data") = nlines_data,
                                    Rcpp::Named("ncovariates") = ncovariates,
                                    Rcpp::Named("data_big") = data_big,
                                    Rcpp::Named("concept") = concept);

  return(L);

}











