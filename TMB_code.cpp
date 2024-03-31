// Simple linear regression.
#include <TMB.hpp>
using namespace density;


template <class Type>
Type objective_function<Type>::operator() ()
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

  PARAMETER_VECTOR(logsigmaepsilon);
  PARAMETER_VECTOR(logsigmadelta);

  // these ones need to be integrated out
  PARAMETER_MATRIX(epsilon);
  PARAMETER_MATRIX(delta);

  // the ll output var
  Type nll = Type(0.0);

  //distributional asignment for the random effects
  for (int i=0; i < epsilon.rows(); i++){
    for (int j=0; j < epsilon.cols(); j++){
      nll -= dnorm(epsilon(i,j), muepsilon(0, i), exp(logsigmaepsilon[i]), true);
    }
  }

  for (int i=0; i < delta.rows(); i++){
    for (int j=0; j < delta.cols(); j++){
     nll -= dnorm(delta(i,j), mudelta(0, i), exp(logsigmadelta[i]), true);
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

  vector<Type> nll_temp(epsilon.rows());

  nll_temp.setZero();

  for(int i=0; i<data.rows(); i++){

    int j = group[i];
    //
    gb2 = gb1 * (delta.transpose().col(j) + mudelta.transpose()) + epsilon.transpose().col(j) + muepsilon.transpose();
    gb3 = (concept * code) * gb2;
    gb3 = exp(gb3.array());

    choiceset_row = data.row(i);

    for(int k = 0; k < choiceset_row.size(); k++){
      prob(k) = gb3(choiceset_row(k)-1, 0);
    }

    prob = prob / prob.sum();

    choice = choices.row(i);

    nll_temp(j) -= dmultinom(choice, //Vector of length K of integers. 0 or 1 for this K will be the max choiceset size, pad with zeros so the extra choices dont do anything
                             prob, //Vector of length K, specifying the probability for the K classes (note, unlike in R these must sum to 1).
                             1); //true if one wants the log-probability, false otherwise.


  }

  nll -= nll_temp.sum();

  // Report objects back to R:
  // ADREPORT(gamma);
  // ADREPORT(beta);
  // ADREPORT(phi);
  //
  // ADREPORT(sigmaepsilon);
  // ADREPORT(sigmadelta);


  // REPORT(gamma);
  // REPORT(beta);
  // REPORT(phi);

  // REPORT(nll_temp);
  // REPORT(epsilon);
  // REPORT(delta);

  return nll;

}

