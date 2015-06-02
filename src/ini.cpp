#include <Rcpp.h>
#include <dplyr.h>
// [[Rcpp::depends(dplyr,BH)]]
#include "handler.h"

// R automatically calls this function when the package is loaded. 
extern "C" void R_init_statar( DllInfo* info ){
  registerHybridHandler( "cov2", cov2_handler );
  /*
  registerHybridHandler( "tlag2", tlag_handler );
  */

}

