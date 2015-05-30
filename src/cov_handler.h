#include <Rcpp.h>
#include <dplyr.h>
#include "cov.h"
using namespace dplyr ;
using namespace Rcpp ;



Result* cov_handler2(SEXP arg1, SEXP arg2, bool narm){
  switch(TYPEOF(arg1)){
    case INTSXP:
      switch(TYPEOF(arg2)){
        case (INTSXP): return new Cov<INTSXP, INTSXP>(arg1, arg2, narm) ;
        case (REALSXP): return new Cov<INTSXP, REALSXP>(arg1, arg2, narm) ;
        default: return 0 ;
      }
    case REALSXP:
    switch(TYPEOF(arg2)){
      case (INTSXP): return new Cov<REALSXP, INTSXP>(arg1, arg2, narm) ;
      case (REALSXP): return new Cov<REALSXP, REALSXP>(arg1, arg2, narm) ;
      default: return 0 ;
    }
    default: return 0 ;
  }
}



Result* cov_handler(SEXP call, const LazySubsets& subsets, int nargs ){
  if( nargs == 0 | nargs >3 ) return 0 ;
  SEXP arg1 = CADR(call) ;
  SEXP arg2 = CADDR(call) ;
  if( TYPEOF(arg1) == SYMSXP ) arg1 = subsets.get_variable(arg1) ;
  if( TYPEOF(arg2) == SYMSXP ) arg2 = subsets.get_variable(arg2) ;
  if (nargs == 2) return cov_handler2(arg1, arg2, FALSE) ;
  if( nargs == 3 ){
    SEXP arg3 = CDDDR(call) ;
    if( TAG(arg3) == R_NaRmSymbol ){
       SEXP narm = CAR(arg3) ;
       if( TYPEOF(narm) == LGLSXP && LENGTH(narm) == 1 ){
          return cov_handler2(arg1, arg2, LOGICAL(narm)[0]) ;
      }
    }
  }
  return 0;
}



