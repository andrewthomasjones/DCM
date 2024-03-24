// Simple linear regression.
#include <TMB.hpp>
template<class Type>

using namespace density;

Type objective_function<Type>::operator() ()
{
  //actual data and design matrices
  DATA_MATRIX(concept);
  DATA_MATRIX(data);
  DATA_MATRIX(code);

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

  // no covariance for now, diagnol variances matrices
  matrix<Type> Sigma_epsilon = sigmaepsilon.asDiagonal();
  matrix<Type> Sigma_delta = sigmadelta.asDiagonal();

  //set up MVnorm objects
  MVNORM_t<Type> mvn_epsilon(Sigma_epsilon);
  MVNORM_t<Type> mvn_delta(Sigma_delta);

  //distributional asignment for the random effects
  nll -= mvn_epsilon(muepsilonparameters).sum();
  nll -= mvn_delta(mudeltaparameters).sum();

  Matrix<Type> imatrix = Matrix<Type, nhop, nhop>::Identity();
  imatrix = imatrix - betaparameters;
  imatrix = imatrix.inverse()

  Matrix<Type> gb = gamma * imatrix;

  gb = gb * delta.transpose() + epsilon.transpose();

  Matrix<Type> concept_use = concept * code;

  gb = concept_use * gb;

  gb = exp(gb);

  for(int i=0; i<ndecisionmakers; i++){ // maybe this is better with data_factor or whatever

    min_i = ; //extent of the data for decsions maker i
    max_i = ; //extent of the data for decsions maker i

    probs = gb.row(data.block(min_i, max_i, 4, nmax_choiceset_size-1) - 1); // pad with zeros ?
    choices = data.block(min_i, max_i, 1, 1) - 1; // remap to 0 to K-1 and fill in correct one with a 1

    // options <- options[options>0]
    // choice <-  which(options == data[i, 2])
    // d_i <- rep(0, length(probs))
    // d_i[choice] <- 1

    for(int j=0; j<probs.rows(); j++){

      nll -= dmultinom(choices.row(j), //Vector of length K of integers. 0 or 1 for this K will be the max choiceset size, pad with zeros so the extra choices dont do anything
                       probs.row(j), //Vector of length K, specifying the probability for the K classes (note, unlike in R these must sum to 1).
                       1 //true if one wants the log-probability, false otherwise.
      )
    }
  }

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


