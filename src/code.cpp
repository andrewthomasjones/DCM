#include "RcppArmadillo.h"

// [[Rcpp::depends(RcppArmadillo)]]

//' @importFrom Rcpp sourceCpp
//' @useDynLib DCM

using namespace Rcpp;

// [[Rcpp::export]]
arma::mat frequencyDistributionCpp(const arma::mat& cold) {
  // init
  int i3 = 0;
  int i4, temp1, temp2;

  int s1_r = cold.n_rows;
  //int s1_c = cold.n_cols;
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
  //double x = accu(fd);

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


// [[Rcpp::export]]
Rcpp::List createConceptsCpp(const arma::mat& data_matrix, int nmax_choiceset_size = 31) {

  int nconcepts, nlines_data, ncovariates, nlines_data_matrix, match_number, match_count;
  int i1, i2, i4, i5, i6;
  int iddm, idcs;

  int ncs4 = nmax_choiceset_size + 4;

  nlines_data_matrix = data_matrix.n_rows;
  ncovariates = data_matrix.n_cols - 3;

  iddm = data_matrix(0, 0);
  idcs = data_matrix(0, 1);

  arma::mat data_big = arma::zeros<arma::mat>(nlines_data_matrix, ncs4);
 //Rcout << "point 1" <<  std::endl;
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

  while (i1 < (nlines_data_matrix-1)){

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

    if (match_number == 0) {
      i6++;
      match_number = i6 +1;
      concept_big.row(i6) = data_matrix(i1, arma::span(3, 3+ncovariates-1));
    }

    //Rcout << "Point 4 " << i1 <<  std::endl;

    if (data_matrix(i1, 0) == iddm) {
      if (data_matrix(i1, 1) == idcs) {
        i5++;
       //Rcout << "Point 4.1 i1 " << i1 << " i5 "<< i5 << " i2 "<< i2 <<  " data big size: "<< data_big.n_rows << " " << data_big.n_cols << std::endl;
        data_big(i2, i5 + 4) = match_number;
       //Rcout << "Point 4.2 i1 " << i1 << " i5 "<< i5 << std::endl;
        if (data_matrix(i1, 2) == 1){
          data_big(i2, 1) = match_number;
         //Rcout << "Point 4.3 " << i1 <<  std::endl;
        }
      }
    }

   //Rcout << "Point 5 " << i1 <<  std::endl;

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
   //Rcout << "Point 6 " << i1 <<  std::endl;
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

 //Rcout << "final point" <<  std::endl;

  nconcepts = i6+1;
  nlines_data = i2+1;

  arma::mat concept = arma::mat(i6+1, ncovariates, arma::fill::zeros);
  concept(arma::span::all, arma::span::all) =
    concept_big(arma::span(0,i6), arma::span(0,ncovariates-1));

  arma::mat data = arma::zeros<arma::mat>(i2+1, (nmax_choiceset_size + 4));
  data(arma::span::all, arma::span::all) =
    data_big(arma::span(0,i2), arma::span(0,ncs4-1));

 //Rcout << "make list" <<  std::endl;

  Rcpp::List L = Rcpp::List::create(Rcpp::Named("data") = data,
                                    Rcpp::Named("nconcepts") = nconcepts,
                                    Rcpp::Named("nlines_data") = nlines_data,
                                    Rcpp::Named("ncovariates") = ncovariates,
                                    Rcpp::Named("data_big") = data_big,
                                    Rcpp::Named("concept") = concept);

  return(L);

}



// cant export
double llCalc_ghq(const arma::vec& working_values,
                  Rcpp::List model,
                  Rcpp::List processed,
                  const arma::mat& ghq_matrix1
){

  arma::mat concept =  as<arma::mat>(processed["concept"]);
  arma::mat data =  as<arma::mat>(processed["data"]);

  int ndecisionmakers = as<int>(processed["ndecisionmakers"]);
  int nmax_choiceset_size = as<int>(processed["nmax_choiceset_size"]);

  int npp = as<int>(model["npp"]);
  int nhop = as<int>(model["nhop"]);

  int integral_size = ghq_matrix1.n_rows;

  arma::mat epsilonmatrix =  as<arma::mat>(model["epsilon"]);
  arma::mat gammamatrix =  as<arma::mat>(model["gamma"]);
  arma::mat deltamatrix =  as<arma::mat>(model["delta"]);
  arma::mat betamatrix =  as<arma::mat>(model["beta"]);
  arma::mat phimatrix =  as<arma::mat>(model["phi"]);
  arma::mat code =  as<arma::mat>(model["code"]);

  arma::vec muepsilonparameters(npp, arma::fill::zeros);
  arma::vec mudeltaparameters(nhop, arma::fill::zeros);
  arma::vec sigmaepsilonparameters(npp, arma::fill::zeros);
  arma::vec sigmadeltaparameters(nhop, arma::fill::zeros);
  //arma::vec deltaepsilonparameters(nhop, arma::fill::zeros);

  arma::mat gammaparameters(arma::size(gammamatrix), arma::fill::zeros);
  arma::mat betaparameters(arma::size(betamatrix), arma::fill::zeros);
  arma::mat phiparameters(arma::size(phimatrix), arma::fill::zeros);

  int m = 0;

  for(int i=0; i<npp; i++){

    if(epsilonmatrix(i,0) == 1){
      muepsilonparameters(i) = working_values[m];
      m++;
    }

    if(epsilonmatrix(i,0) == -1){
      muepsilonparameters(i) = 1;
    }

  }

  for(int i=0; i<nhop; i++){

    if(deltamatrix(i,0) == 1){
      mudeltaparameters(i) = working_values[m];
      m++;
    }

    if(deltamatrix(i,0) == -1){
      mudeltaparameters(i) = 1;
    }
  }


  for(int i=0; i<npp; i++){

    if(epsilonmatrix(i,1)==1){
      sigmaepsilonparameters(i) = abs(working_values[m]);
      m++;
    }

    if(epsilonmatrix(i,1) == -1){
      sigmaepsilonparameters(i) = 1;
    }
  }



  for(int i=0; i<nhop; i++){

    if(deltamatrix(i, 1) == 1){
      sigmadeltaparameters(i) = abs(working_values[m]);
      m++;
    }

    if(deltamatrix(i, 1) == -1){
      sigmadeltaparameters(i) = 1;
    }
  }



  for(int j=0; j<nhop; j++){
    for(int i=0; i<npp; i++){

      if(gammamatrix(i,j) == 1){
        gammaparameters(i,j) = working_values[m];
        m++;
      }
      if(gammamatrix(i,j) == -1){
        gammaparameters(i,j) = 1;
      }
    }
  }



  for(int j=0; j<nhop; j++){
    for(int i=0; i<nhop; i++){

      if(betamatrix(i,j)==1){
        betaparameters(i,j) = working_values[m];
        m++;
      }
      if(betamatrix(i,j)==-1){
        betaparameters(i,j) = 1;
      }
    }
  }



  // phiparameters.diag().ones();
  //
  // for(int i=0; i < (npp+nhop-1); i++){
  //   for(int j=i+1; j < (npp+nhop); j++){
  //     if(phimatrix(i, j) == 1){
  //       if(phimatrix(j, i) == 1){
  //         phiparameters(i, j) = working_values[m];
  //         phiparameters(j, i) = working_values[m];
  //         m++;
  //       }
  //     }
  //   }
  // }
  //
  // ghq_matrix1 *= arma::pow(phiparameters, 0.5);

  arma::mat int_epsilon = ghq_matrix1.cols(1, npp);

  arma::mat int_delta = ghq_matrix1.cols(npp+1, nhop+npp);

  int_epsilon *= arma::diagmat((sigmaepsilonparameters));
  int_epsilon.each_row() += muepsilonparameters.t();

  int_delta *= arma::diagmat((sigmadeltaparameters));
  int_delta.each_row() += mudeltaparameters.t();

  ///////////////////////////////////////////////////
  arma::mat imatrix = arma::eye(nhop, nhop);
  arma::mat gb(nhop, nhop, arma::fill::zeros);

  gb = gammaparameters*arma::inv(imatrix-betaparameters);
  gb = gb * int_delta.t() + int_epsilon.t();

  arma::mat concept_use = concept * code;
  gb = concept_use * gb;
  gb = arma::exp(gb);

  arma::vec pthisdm(integral_size, arma::fill::ones);
  arma::vec pthiscs(integral_size, arma::fill::zeros);
  arma::vec ploglike(ndecisionmakers, arma::fill::zeros);
  arma::vec bottom(gb.n_cols, arma::fill::zeros);

  int n = 0;
  double iddm  = data(0, 0);
  int nlines = data.n_rows;

  for(int i = 0; i < nlines; i++){

    bottom.zeros();

    for(int j = 0; j < nmax_choiceset_size; j++){
      if(data(i, j + 4) > 0){
        bottom  =  bottom + arma::trans(gb.row(data(i, j + 4) - 1));
      }
    }



    pthiscs  =  arma::trans(gb.row(data(i, 1) - 1)) / bottom;

    if(data(i, 0) == iddm){
      pthisdm  =  pthisdm % pthiscs;
    }


    if(data(i, 0) > iddm){
      ploglike(n) = arma::accu(pthisdm) / integral_size;
      n++;
      pthisdm  =  pthiscs;
      iddm  =  data(i, 0);
    }


  }

  ploglike(n) = arma::accu(pthisdm) / integral_size;

  double loglike  =  -1.0 * arma::accu(arma::log(ploglike));

  return(loglike);

}


//' llMax_ghq
//' does the maximisation
//' @param model list
//' @param processed  list
//' @param ghq_matrix1 matrix
//' @param nlm_params list of params
//' @returns opt_results
//' @export
// [[Rcpp::export]]
Rcpp::List llMax_ghq(Rcpp::List model,
                      Rcpp::List processed,
                      const arma::mat& ghq_matrix1,
                      Rcpp::List nlm_params){


   arma::vec working_values =  as<arma::vec>(model["initial_values"]);

   // Extract R's optim function
   Rcpp::Environment stats("package:stats");
   Rcpp::Function nlm = stats["nlm"];


   // Call the optim function from R in C++
   Rcpp::List opt_results = nlm(Rcpp::_["f"] = Rcpp::InternalFunction(&llCalc_ghq),
                                Rcpp::_["p"] = working_values,
                                Rcpp::_["model"] = model,
                                Rcpp::_["processed"] = processed,
                                Rcpp::_["hessian"] = true,
                                Rcpp::_["print.level"] = nlm_params["verbose"],
                                Rcpp::_["iterlim"] = 1000,
                                Rcpp::_["ghq_matrix1"] = ghq_matrix1,
                                Rcpp::_["gradtol"] = nlm_params["gradtol"],
                                Rcpp::_["stepmax"] = nlm_params["stepmax"],
                                Rcpp::_["steptol"] = nlm_params["steptol"]);

   // Return estimated values
   return opt_results ;
}



// cant export normall
// [[Rcpp::export]]
double llCalc_ghq_e(const arma::vec& working_values,
                  Rcpp::List model,
                  Rcpp::List processed,
                  const arma::mat& ghq_matrix1){

    double out = llCalc_ghq(working_values,
                model,
                processed,
                ghq_matrix1);

    return(out);
}




