/// @file DCMLL.hpp

using namespace density;

#ifndef DCMLL_hpp
#define DCMLL_hpp

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

template <class Type>
Type DCMLL(objective_function<Type>* obj)
{
  //actual data and design matrices
  DATA_MATRIX(concept);
  DATA_IMATRIX(data);
  DATA_MATRIX(code);
  DATA_FACTOR(group); //decsion makers
  DATA_MATRIX(choices); //matrix of choices - each row all zeros but has one 1
  DATA_MATRIX(imatrix);

  //parameter matrices
  PARAMETER_MATRIX(gamma);
  PARAMETER_MATRIX(beta);
  PARAMETER_MATRIX(phi);

  PARAMETER_MATRIX(muepsilon);
  PARAMETER_MATRIX(mudelta);

  PARAMETER_MATRIX(logsigmaepsilon);
  PARAMETER_MATRIX(logsigmadelta);

  // these ones need to be integrated out
  PARAMETER_MATRIX(epsilon);
  PARAMETER_MATRIX(delta);

  // the ll output var
  Type nll = Type(0.0);

  Type nll_epsilon = Type(0.0);
  Type nll_delta = Type(0.0);

  matrix<Type> sigmaepsilon = logsigmaepsilon.array().exp();
  matrix<Type> sigmadelta = logsigmadelta.array().exp();

  //distributional asignment for the random effects
  for (int i=0; i < epsilon.rows(); i++){
    for (int j=0; j < epsilon.cols(); j++){
      nll_epsilon += dnorm(epsilon(i,j), muepsilon(0, j), sigmaepsilon(0, j), 1);
    }
  }

  for (int i=0; i < delta.rows(); i++){
    for (int j=0; j < delta.cols(); j++){
      nll_delta += dnorm(delta(i,j), mudelta(0, j), sigmadelta(0, j), 1);
    }
  }

  imatrix = imatrix - beta;
  imatrix = imatrix.inverse();

  matrix<Type> gb1 = gamma * imatrix;

  vector<Type> prob(data.cols());
  vector<Type> choice(data.cols());
  vector<int> choiceset_row;

  matrix<Type> gb2;
  matrix<Type> gb3;
  //matrix<Type> gb3_exp;

  vector<Type> nll_data(epsilon.rows());

  nll_data.setZero();

  for(int i=0; i<data.rows(); i++){

    int j = group[i];
    //
    gb2 = gb1 * (delta.transpose().col(j)) + epsilon.transpose().col(j);
    gb3 = (concept * code) * gb2;
    gb3 = exp(gb3.array());

    choiceset_row = data.row(i);

    for(int k = 0; k < choiceset_row.size(); k++){
      if(choiceset_row(k)-1 >= 0){
        prob(k) = gb3(choiceset_row(k)-1, 0);
      }else{
        prob(k) = 0.0000001; //FIXME
      }
    }

    prob = prob / prob.sum();

    choice = choices.row(i);

    nll_data(j) += dmultinom(choice, //Vector of length K of integers. 0 or 1 for this K will be the max choiceset size, pad with zeros so the extra choices dont do anything
                             prob, //Vector of length K, specifying the probability for the K classes (note, unlike in R these must sum to 1).
                             1); //true if one wants the log-probability, false otherwise.


  }

  nll -= nll_data.sum();
  nll -= nll_epsilon;
  nll -= nll_delta;

  REPORT(sigmaepsilon);
  REPORT(sigmadelta);

  ADREPORT(sigmaepsilon);
  ADREPORT(sigmadelta);

  // Type test = sigmadelta(0,0);
  // ADREPORT(test);
  // REPORT(test);
  // Type test2 = exp(logsigmadelta(0,0);
  // ADREPORT(test2);
  // REPORT(test);

  // Report objects back to R:
  // ADREPORT(gamma);
  // ADREPORT(beta);
  // ADREPORT(phi);
  // ADREPORT(mudelta);
  // ADREPORT(muepsilon);
  //
  // REPORT(nll_data);
  // REPORT(nll_delta);
  // REPORT(nll_epsilon);
  //
  // REPORT(epsilon);
  // REPORT(delta);

  return nll;

}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this

#endif
