#define TMB_LIB_INIT R_init_DCM_TMBExports
#include <TMB.hpp>
#include "DCMLL.hpp"

template<class Type>
Type objective_function<Type>::operator() () {
  DATA_STRING(model);
  if(model == "DCMLL") {
    return DCMLL(this);
  } else {
    Rf_error("Unknown model.");
  }
  return 0;
}
