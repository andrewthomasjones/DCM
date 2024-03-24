// Simple linear regression.
#include <TMB.hpp>
template<class Type>

using namespace density;


Type logsumexp(Type x){
  c = x.maxCoeff();
  return c + log(exp(x - c).sum());
}





Type objective_function<Type>::operator() ()
{
  //actual data and design matrices
  DATA_MATRIX(concept);
  DATA_MATRIX(data);
  DATA_MATRIX(code);
  DATA_FACTOR(group); //decsion makers
  DATA_MATRIX(choices); //matrix of choices - each row all zeros but has one 1

  //model specs
  DATA_INTEGER(nmax_choiceset_size);
  DATA_INTEGER(ndecisionmakers);
  DATA_INTEGER(npp);
  DATA_INTEGER(nhop);

  //parameter matrices
  PARAMETER_MATRIX(gamma);
  PARAMETER_MATRIX(beta);
  PARAMETER_MATRIX(phi);

  // these ones need to be integrated out
  PARAMETER_VECTOR(muepsilon);
  PARAMETER_VECTOR(mudelta);

  // and are parametized here
  PARAMETER_VECTOR(sigmaepsilon);
  PARAMETER_VECTOR(sigmadelta);
  PARAMETER_MATRIX(epsilon);
  PARAMETER_MATRIX(delta);

  // the ll output var
  Type nll = Type(0);
  vector<Type> nll_temp = vector<Type>::Zeros(ndecisionmakers);

  // no covariance for now, diagnol variances matrices
  matrix<Type> Sigma_epsilon = sigmaepsilon.asDiagonal();
  matrix<Type> Sigma_delta = sigmadelta.asDiagonal();

  //set up MVnorm objects
  MVNORM_t<Type> mvn_epsilon(Sigma_epsilon);
  MVNORM_t<Type> mvn_delta(Sigma_delta);

  //distributional asignment for the random effects
  nll -= mvn_epsilon(epsilon-muepsilon).sum();
  nll -= mvn_delta(delta-mudelta).sum();

  Matrix<Type> imatrix = Matrix<Type, nhop, nhop>::Identity();
  imatrix = imatrix - betaparameters;
  imatrix = imatrix.inverse()

  Matrix<Type> gb = gamma * imatrix;

  gb = gb * delta.transpose() + epsilon.transpose();

  Matrix<Type> concept_use = concept * code;

  gb = concept_use * gb;

  gb = exp(gb);


  for(int i=0; i<data.rows(); i++){

    j = group[i];

    probs = gb.row(data.block(i, i, 4, nmax_choiceset_size-1) - 1); // pad with zeros ? check is actually a vector?

    nll_temp[j] += dmultinom(choices.row(1), //Vector of length K of integers. 0 or 1 for this K will be the max choiceset size, pad with zeros so the extra choices dont do anything
                       probs, //Vector of length K, specifying the probability for the K classes (note, unlike in R these must sum to 1).
                       1) //true if one wants the log-probability, false otherwise.
  }

  nll -= logsumexp(nll_temp);

  // Report objects back to R:
  ADREPORT(gamma);
  ADREPORT(beta);
  ADREPORT(phi);

  ADREPORT(sigmaepsilon);
  ADREPORT(sigmadelta);
  ADREPORT(muepsilon);
  ADREPORT(mudelta);

  REPORT(gamma);
  REPORT(beta);
  REPORT(phi);

  REPORT(sigmaepsilon);
  REPORT(sigmadelta);
  REPORT(muepsilon);
  REPORT(mudelta);

  REPORT(epsilon);
  REPORT(delta);

  return nll;

}


