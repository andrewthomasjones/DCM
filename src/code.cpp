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
 //Rcout << "point 2" <<  std::endl;
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

//' llCalc3
//' does the calc
//' actually llCalc3a does, this is same but other cant export
//' @param working_values vector
//' @param model List
//' @param processed List
//' @param gq_int_matrix matrix
//' @returns loglike
//' @export
// [[Rcpp::export]]
double llCalc3(const arma::vec& working_values,
                  Rcpp::List model,
                  Rcpp::List processed,
                  const arma::mat& gq_int_matrix){


  arma::mat concept =  as<arma::mat>(processed["concept"]);
  arma::mat data =  as<arma::mat>(processed["data"]);

  int ndecisionmakers = as<int>(processed["ndecisionmakers"]);
  int nmax_choiceset_size = as<int>(processed["nmax_choiceset_size"]);

  int npp = as<int>(model["npp"]);
  int nhop = as<int>(model["nhop"]);

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
  arma::vec deltaepsilonparameters(nhop, arma::fill::zeros);

  arma::mat gammaparameters(arma::size(gammamatrix), arma::fill::zeros);
  arma::mat betaparameters(arma::size(betamatrix), arma::fill::zeros);
  arma::mat phiparameters(arma::size(phimatrix), arma::fill::zeros);

  int m = 0;
  int integral_size = gq_int_matrix.n_rows;


  for(int i=0; i<npp; i++){

    if(epsilonmatrix(i,0)==1){
      muepsilonparameters(i) = working_values(m);
      m++;
    }

    if(epsilonmatrix(i,0) == -1){
      muepsilonparameters(i) = 1;
    }

  }

  for(int i=0; i<nhop; i++){

    if(deltamatrix(i,0)==1){
      mudeltaparameters(i) = working_values[m];
      m++;
    }

    if(deltamatrix(i,0)==-1){
      deltaepsilonparameters(i) = 1;
    }
  }

  for(int i=0; i<npp; i++){

    if(epsilonmatrix(i,1)==1){
      sigmaepsilonparameters(i) = abs(working_values[m]);
      m++;
    }

    if(epsilonmatrix(i,1)==-1){
      sigmaepsilonparameters(i) = 1;
    }
  }

  for(int i=0; i<nhop; i++){

    if(deltamatrix(i, 1)==1){
      sigmadeltaparameters(i) = abs(working_values[m]);
      m++;
    }

    if(deltamatrix(i,1)==-1){
      sigmadeltaparameters(i) = 1;
    }
  }

  for(int j=0; j<nhop; j++){
    for(int i=0; i<npp; i++){

      if(gammamatrix(i,j)==1){
        gammaparameters(i,j) = working_values[m];
        m++;
      }
      if(gammamatrix(i,j)==-1){
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

  phiparameters.diag().ones();

  for(int i=0; i<(npp+nhop-1); i++){
    for(int j=i+1; j<(npp+nhop); j++){
      if(phimatrix(i,j)==1){
        if(phimatrix(j,i)==1){
          phiparameters(i,j) = working_values[m];
          phiparameters(j,i) = working_values[m];
          m++;
        }
      }
    }
  }

  arma::mat gq_int_matrix2 = gq_int_matrix * pow( phiparameters, 0.5);
  arma::mat int_epsilon = gq_int_matrix2.cols(0, npp-1);
  arma::mat int_delta = gq_int_matrix2.cols(npp, npp+nhop-1);

  for(int i=0; i<integral_size; i++){

    for(int j=0; j<npp; j++){
      int_epsilon(i,j) = int_epsilon(i,j)*sigmaepsilonparameters[j]+muepsilonparameters[j];
    }

    for(int j=0; j<nhop; j++){
      int_delta(i,j) = int_delta(i,j)*sigmadeltaparameters[j]+mudeltaparameters[j];
    }

  }

  arma::mat imatrix = arma::eye(nhop, nhop);
  arma::mat gb(nhop, nhop, arma::fill::zeros);

  gb = gammaparameters*arma::inv(imatrix-betaparameters);
  gb = gb*int_delta.t() + int_epsilon.t();

  arma::mat concept_use = concept * code;
  gb = concept_use*gb;
  gb = arma::exp(gb);

  arma::vec pthisdm(integral_size, arma::fill::ones);
  arma::vec pthiscs(integral_size, arma::fill::zeros);
  arma::vec ploglike(ndecisionmakers, arma::fill::zeros);
  arma::vec bottom(gb.n_cols, arma::fill::zeros);

  int n = 0;
  double iddm  = data(0,0);
  int nlines = data.n_rows;

  for(int i=0; i<nlines; i++){

    bottom.zeros();

    for(int j=0; j<nmax_choiceset_size; j++){
      if(data(i,j+4)>0){
        bottom  =  bottom+arma::trans(gb.row(data(i,j+4)-1));
      }
    }



    pthiscs  =  arma::trans(gb.row(data(i,1)-1))/bottom;

    if(data(i, 0)==iddm){
      pthisdm  =  pthisdm%pthiscs;
    }


    if(data(i, 0)>iddm){
      ploglike(n) = arma::accu(pthisdm)/integral_size;
      n++;
      pthisdm  =  pthiscs;
      iddm  =  data(i, 0);
    }


  }

  ploglike(n) = arma::accu(pthisdm)/integral_size;


  double loglike  =  -1.0 * arma::accu(arma::log(ploglike));

  return(loglike);

}


double llCalc3a(const arma::vec& working_values,
               Rcpp::List model,
               Rcpp::List processed,
               const arma::mat& gq_int_matrix){

  arma::mat concept =  as<arma::mat>(processed["concept"]);
  arma::mat data =  as<arma::mat>(processed["data"]);

  int ndecisionmakers = as<int>(processed["ndecisionmakers"]);
  int nmax_choiceset_size = as<int>(processed["nmax_choiceset_size"]);

  int npp = as<int>(model["npp"]);
  int nhop = as<int>(model["nhop"]);

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
  arma::vec deltaepsilonparameters(nhop, arma::fill::zeros);

  arma::mat gammaparameters(arma::size(gammamatrix), arma::fill::zeros);
  arma::mat betaparameters(arma::size(betamatrix), arma::fill::zeros);
  arma::mat phiparameters(arma::size(phimatrix), arma::fill::zeros);

  int integral_size = gq_int_matrix.n_rows;
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
      deltaepsilonparameters(i) = 1;
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

  phiparameters.diag().ones();

  for(int i=0; i < (npp+nhop-1); i++){
    for(int j=i+1; j < (npp+nhop); j++){
      if(phimatrix(i, j) == 1){
        if(phimatrix(j, i) == 1){
          phiparameters(i, j) = working_values[m];
          phiparameters(j, i) = working_values[m];
          m++;
        }
      }
    }
  }

  arma::mat gq_int_matrix2 = gq_int_matrix * pow( phiparameters, 0.5);
  arma::mat int_epsilon = gq_int_matrix2.cols(0, npp-1);
  arma::mat int_delta = gq_int_matrix2.cols(npp, npp+nhop-1);

  for(int i=0; i<integral_size; i++){

    for(int j=0; j<npp; j++){
      int_epsilon(i,j) = int_epsilon(i,j)*sigmaepsilonparameters[j]+muepsilonparameters[j];
    }

    for(int j=0; j<nhop; j++){
      int_delta(i,j) = int_delta(i,j)*sigmadeltaparameters[j]+mudeltaparameters[j];
    }

  }

  arma::mat imatrix = arma::eye(nhop, nhop);
  arma::mat gb(nhop, nhop, arma::fill::zeros);

  gb = gammaparameters*arma::inv(imatrix-betaparameters);
  gb = gb*int_delta.t() + int_epsilon.t();

  arma::mat concept_use = concept * code;
  gb = concept_use*gb;
  gb = arma::exp(gb);

  arma::vec pthisdm(integral_size, arma::fill::ones);
  arma::vec pthiscs(integral_size, arma::fill::zeros);
  arma::vec ploglike(ndecisionmakers, arma::fill::zeros);
  arma::vec bottom(gb.n_cols, arma::fill::zeros);

  int n = 0;
  double iddm  = data(0,0);
  int nlines = data.n_rows;

  for(int i=0; i<nlines; i++){

    bottom.zeros();

    for(int j=0; j<nmax_choiceset_size; j++){
      if(data(i,j+4)>0){
        bottom  =  bottom+arma::trans(gb.row(data(i,j+4)-1));
      }
    }



    pthiscs  =  arma::trans(gb.row(data(i,1)-1))/bottom;

    if(data(i, 0)==iddm){
      pthisdm  =  pthisdm%pthiscs;
    }


    if(data(i, 0)>iddm){
      ploglike(n) = arma::accu(pthisdm)/integral_size;
      n++;
      pthisdm  =  pthiscs;
      iddm  =  data(i, 0);
    }


  }

  ploglike(n) = arma::accu(pthisdm)/integral_size;


  double loglike  =  -1.0 * arma::accu(arma::log(ploglike));

  return(loglike);

}


//' llMax2
//' does the maximisation
//' @param model list
//' @param processed  list
//' @param gq_int_matrix matrix
//' @param nlm_params list of params
//' @returns opt_results
//' @export
// [[Rcpp::export]]
Rcpp::List llMax2( Rcpp::List model,
                   Rcpp::List processed,
                   const arma::mat& gq_int_matrix,
                   Rcpp::List nlm_params){


  arma::vec working_values =  as<arma::vec>(model["initial_values"]);

  // Extract R's optim function
  Rcpp::Environment stats("package:stats");
  Rcpp::Function nlm = stats["nlm"];


  // Call the optim function from R in C++
  Rcpp::List opt_results = nlm(Rcpp::_["f"] = Rcpp::InternalFunction(&llCalc3a),
                               Rcpp::_["p"] = working_values,
                               Rcpp::_["model"] = model,
                               Rcpp::_["processed"] = processed,
                               Rcpp::_["hessian"] = true,
                               Rcpp::_["print.level"] = nlm_params["verbose"],
                               Rcpp::_["iterlim"] = 1000,
                               Rcpp::_["gq_int_matrix"] = gq_int_matrix,
                               Rcpp::_["gradtol"] = nlm_params["gradtol"],
                               Rcpp::_["stepmax"] = nlm_params["stepmax"],
                               Rcpp::_["steptol"] = nlm_params["steptol"]);

  // Return estimated values
  return opt_results ;
}


// cant export
double llCalc_ghq(const arma::vec& working_values,
                Rcpp::List model,
                Rcpp::List processed,
                const arma::mat& gqh_matrix1,
                const arma::mat& gqh_matrix2
                ){

  arma::mat concept =  as<arma::mat>(processed["concept"]);
  arma::mat data =  as<arma::mat>(processed["data"]);

  int ndecisionmakers = as<int>(processed["ndecisionmakers"]);
  int nmax_choiceset_size = as<int>(processed["nmax_choiceset_size"]);

  int npp = as<int>(model["npp"]);
  int nhop = as<int>(model["nhop"]);

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
  arma::vec deltaepsilonparameters(nhop, arma::fill::zeros);

  arma::mat gammaparameters(arma::size(gammamatrix), arma::fill::zeros);
  arma::mat betaparameters(arma::size(betamatrix), arma::fill::zeros);
  arma::mat phiparameters(arma::size(phimatrix), arma::fill::zeros);

  int integral_size1 = gqh_matrix1.n_rows;
  int integral_size2 = gqh_matrix2.n_rows;

  int data_size = data.n_rows;


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
      deltaepsilonparameters(i) = 1;
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
  // arma::mat gq_int_matrix2 = gq_int_matrix * pow(phiparameters, 0.5);

  arma::mat int_delta = gqh_matrix1.cols(1, nhop);
  arma::mat int_epsilon = gqh_matrix2.cols(1, npp);

  int_epsilon *= arma::diagmat((sigmaepsilonparameters));
  int_epsilon.each_row() += muepsilonparameters.t();

  int_delta *= arma::diagmat((sigmadeltaparameters));
  int_delta.each_row() += mudeltaparameters.t();

  arma::mat imatrix = arma::eye(nhop, nhop);
  arma::mat gb(nhop, nhop, arma::fill::zeros);
  arma::mat concept_use = concept * code;

  gb = gammaparameters*arma::inv(imatrix-betaparameters);
  gb = gb*int_delta.t();
  gb = concept_use*gb;

  arma::mat ec(arma::size(int_epsilon.t()), arma::fill::zeros);
  ec = int_epsilon.t();
  ec = concept_use*ec;

  arma::vec decisionmakers = arma::unique(data.col(0));
  arma::mat prob_temp_gb(ndecisionmakers, integral_size1, arma::fill::zeros);
  arma::mat prob_temp_ec(ndecisionmakers, integral_size2, arma::fill::zeros);

  arma::vec w1 = gqh_matrix1.col(0);
  arma::vec w2 = gqh_matrix2.col(0);

  for(int i=0; i<data_size; i++){

      arma::uvec desc_idx_a  = arma::find(decisionmakers == data(i, 0));
      Rcout << "desc_idx_a.n_elem " << desc_idx_a.n_elem <<  std::endl;
      int desc_idx = desc_idx_a(0); //should only be one

      arma::uvec set_list_a = arma::conv_to<arma::uvec>::from(data(i, arma::span(3, nmax_choiceset_size + 3)));
      arma::uvec set_list =  set_list_a.elem(arma::find(set_list_a > 0)) - 1;

          // Rcout << "i " << data(i, 1) <<  std::endl;
          // Rcout << "data(i, 1) " << data(i, 1) <<  std::endl;
          // Rcout << "set_list " << set_list.t() <<  std::endl;
          // Rcout << "gb.n_rows " << gb.n_rows <<  std::endl;
          // Rcout << "gb.n_cols " << gb.n_cols <<  std::endl;
          //
          // arma::mat temp = gb.rows(set_list);
          //
          // Rcout << "temp.n_rows " << temp.n_rows <<  std::endl;
          // Rcout << "temp.n_cols " << temp.n_cols <<  std::endl;

          //arma::mat temp2 = arma::log(arma::sum(arma::exp(temp)));

      arma::rowvec pthiscs_gb = gb.row(data(i, 1) - 1) - arma::log(arma::sum(arma::exp(gb.rows(set_list)), 0));
      prob_temp_gb.row(desc_idx) +=  pthiscs_gb;

      arma::rowvec pthiscs_ec = ec.row(data(i, 1) - 1) - arma::log(arma::sum(arma::exp(ec.rows(set_list)), 0));
      prob_temp_ec.row(desc_idx) +=  pthiscs_ec;

    }

    prob_temp_gb = arma::exp(prob_temp_gb);
    arma::mat prob_temp_gb2 =  prob_temp_gb * arma::diagmat(w1);
    arma::vec nd_prob_gb = arma::sum(prob_temp_gb2, 1) / arma::accu(w1);

    prob_temp_ec = arma::exp(prob_temp_ec);
    arma::mat prob_temp_ec2 =  prob_temp_ec * arma::diagmat(w2);
    arma::vec nd_prob_ec = arma::sum(prob_temp_ec2, 1) / arma::accu(w2);

    arma::vec nd_prob = nd_prob_gb + nd_prob_ec;
    double loglike = -1.0*arma::sum(arma::log(nd_prob));

    return(loglike);

  }



// cant export
double llCalc_ghq2(const arma::vec& working_values,
                  Rcpp::List model,
                  Rcpp::List processed,
                  const arma::mat& gqh_matrix1
){

  arma::mat concept =  as<arma::mat>(processed["concept"]);
  arma::mat data =  as<arma::mat>(processed["data"]);

  int ndecisionmakers = as<int>(processed["ndecisionmakers"]);
  int nmax_choiceset_size = as<int>(processed["nmax_choiceset_size"]);

  int npp = as<int>(model["npp"]);
  int nhop = as<int>(model["nhop"]);

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
  arma::vec deltaepsilonparameters(nhop, arma::fill::zeros);

  arma::mat gammaparameters(arma::size(gammamatrix), arma::fill::zeros);
  arma::mat betaparameters(arma::size(betamatrix), arma::fill::zeros);
  arma::mat phiparameters(arma::size(phimatrix), arma::fill::zeros);

  int integral_size1 = gqh_matrix1.n_rows;
 //int integral_size2 = gqh_matrix2.n_rows;

  int data_size = data.n_rows;


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
      deltaepsilonparameters(i) = 1;
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
  // arma::mat gq_int_matrix2 = gq_int_matrix * pow(phiparameters, 0.5);

  arma::mat int_delta = gqh_matrix1.cols(1, nhop);
  arma::mat int_epsilon = gqh_matrix1.cols(nhop+1, nhop+npp);

  int_epsilon *= arma::diagmat((sigmaepsilonparameters));
  int_epsilon.each_row() += muepsilonparameters.t();

  int_delta *= arma::diagmat((sigmadeltaparameters));
  int_delta.each_row() += mudeltaparameters.t();

  arma::mat imatrix = arma::eye(nhop, nhop);
  arma::mat gb(nhop, nhop, arma::fill::zeros);
  arma::mat concept_use = concept * code;

  gb = gammaparameters*arma::inv(imatrix-betaparameters);
  gb = gb*int_delta.t() + int_epsilon.t();
  gb = concept_use*gb;

  arma::vec decisionmakers = arma::unique(data.col(0));
  int decisionmakers_n = decisionmakers.n_elem;

  // arma::mat prob_temp_gb(ndecisionmakers, integral_size1, arma::fill::zeros);
  // arma::mat prob_temp_ec(ndecisionmakers, integral_size2, arma::fill::zeros);

  arma::vec w1 = gqh_matrix1.col(0);

  arma::vec ll_n(decisionmakers_n, arma::fill::zeros);

  for(int n=0; n<decisionmakers_n; n++){
    arma::mat subset = data.rows(arma::find(data.col(0) == decisionmakers(n)));
    int a_n = subset.n_rows;

    arma::mat prob_temp(a_n, integral_size1, arma::fill::zeros);

    for(int j=0; j<subset.n_rows; j++){
      arma::uvec set_list_a = arma::conv_to<arma::uvec>::from(subset(j, arma::span(3, nmax_choiceset_size + 3)));
      arma::uvec set_list =  set_list_a.elem(arma::find(set_list_a > 0)) - 1;

      prob_temp.row(j) = gb.row(subset(j, 1) - 1) - arma::log(arma::sum(arma::exp(gb.rows(set_list)), 0));
    }

    ll_n(n) = arma::dot(arma::exp(arma::sum(prob_temp_A, 0)), w1)/arma::accu(w1);

  }

  double loglike = -1.0*arma::sum(arma::log(ll_n));

  return(loglike);

}




// //' llCalc_ghq_e
//  //' does the calc
//  //' llCalc_ghq does, this is same but other cant export
//  //' @param working_values vector
//  //' @param model List
//  //' @param processed List
//  //' @param gqh_matrix1 matrix
//  //' @param gqh_matrix2 matrix
//  //' @returns loglike
//  //' @export
//  // [[Rcpp::export]]
//  double llCalc_ghq_e(const arma::vec& working_values,
//                      Rcpp::List model,
//                      Rcpp::List processed,
//                      const arma::mat& gqh_matrix1,
//                      const arma::mat& gqh_matrix2){
//
//    return(llCalc_ghq(working_values, model,processed, gqh_matrix1, gqh_matrix2));
//
//  }



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
   Rcpp::List opt_results = nlm(Rcpp::_["f"] = Rcpp::InternalFunction(&llCalc_ghq2),
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








