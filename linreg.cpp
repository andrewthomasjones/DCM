// Simple linear regression.
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(Y);
  DATA_MATRIX(X);

  PARAMETER_VECTOR(muepsilonparameters);
  PARAMETER_VECTOR(sigmaepsilonparameters);

  Type nll = 0;

  nll += dnorm(Y, muepsilonparameters, sigmaepsilonparameters, true).sum()

  return nll;
}
