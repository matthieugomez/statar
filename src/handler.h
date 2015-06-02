#include <Rcpp.h>
#include <dplyr.h>
#include "cov.h"
#include "tlag.h"

using namespace dplyr ;
using namespace Rcpp ;


template <bool narm>
Result* cov2_handler2(SEXP arg1, SEXP arg2){
  switch(TYPEOF(arg1)){
    case INTSXP:
      switch(TYPEOF(arg2)){
        case (INTSXP): return new Cov2<INTSXP, INTSXP, narm>(arg1, arg2) ;
        case (REALSXP): return new Cov2<INTSXP, REALSXP, narm>(arg1, arg2) ;
        default: return 0 ;
      }
    case REALSXP:
    switch(TYPEOF(arg2)){
      case (INTSXP): return new Cov2<REALSXP, INTSXP, narm>(arg1, arg2) ;
      case (REALSXP): return new Cov2<REALSXP, REALSXP, narm>(arg1, arg2) ;
      default: return 0 ;
    }
    default: return 0 ;
  }
}



Result* cov2_handler(SEXP call, const LazySubsets& subsets, int nargs ){
  if( nargs == 0 | nargs >3 ) return 0 ;
  SEXP arg1 = CADR(call) ;
  SEXP arg2 = CADDR(call) ;
  if( TYPEOF(arg1) == SYMSXP ) arg1 = subsets.get_variable(arg1) ;
  if( TYPEOF(arg2) == SYMSXP ) arg2 = subsets.get_variable(arg2) ;
  if (nargs == 2) return cov2_handler2<false>(arg1, arg2) ;
  if( nargs == 3 ){
    SEXP arg3 = CDDDR(call) ;
    if( TAG(arg3) == R_NaRmSymbol ){
       SEXP narm = CAR(arg3) ;
       if( TYPEOF(narm) == LGLSXP && LENGTH(narm) == 1 ){
          if( LOGICAL(narm)[0] == TRUE ){
              return cov2_handler2<true>(arg1, arg2) ;
          } else {
              return cov2_handler2<false>(arg1, arg2) ;
          }
      }
    }
  }
  return 0;
}

/*
Result* tlag_handler(SEXP call, const LazySubsets& subsets, int nargs ){
  if( nargs == 0 | nargs >3 ) return 0 ;
  SEXP p = CDR(call) ;
  SEXP tag = TAG(p) ;
  int n = 1;
  SEXP along_with;
  SEXP x = CAR(p) ;
  if( TYPEOF(x) == SYMSXP ) x = subsets.get_variable(x) ;
  p = CDR(p) ;
  tag = TAG(p) ;
  try{
      n = as<int>( CAR(p) );
  } catch( ... ){
      SEXP n_ = CADDR(call);
      std::stringstream s ;
      stop( "could not convert second argument to an integer. type=%s, length = %d",
          type2name(n_), Rf_length(n_) ) ;
  }
  p = CDR(p) ;
  along_with = CAR(p);
  if( TYPEOF(along_with) == SYMSXP ) along_with = subsets.get_variable(along_with) ;
  return new Tlag<REALSXP>(x, n, along_with) ;
}
  


if( tag == R_NilValue ) stop( "all arguments of 'first' after the first one should be named" ) ;
*/



/* test
df <- data_frame(id = c(1, 1, 2 ,2 , 2, 3, 3, 3), 
                v1 = c(NA, 3, 2, 1, 5, 6, 7 , 8),
                v2 = c(1, 3, 5, 7, NA, 8, 3, 1)
              )
df %>% group_by(id) %>% summarize(temp = cov(v1, v2, use = "everything"))
df %>% group_by(id) %>% summarize(temp = cov(v1, v2, use = "na.or.complete"))
df %>% group_by(id) %>% summarize(temp = cov2(v1, v2, na.rm = FALSE))
df %>% group_by(id) %>% summarize(temp = cov2(v1, v2, na.rm = TRUE))

tlag <- function(x, n = 1L, along_with, default = NA) { 
  if (!is.numeric(n) | (length(n)>1)) stop("n must be a numeric of length one")
  index <- match(along_with - n, along_with, incomparables = NA)
  out <- x[index]
  if (!is.na(default)) out[which(is.na(index))] <- default
  out
}

df <- data_frame(
year = c(1989L, 1990L,  1991L, 1992L),
value = c(4.1, 4.5, 3.2, 3.3)
)
df %>% mutate(value_lag = tlag2(value, 1, year))

lag(value, 1, order_by = year)
# [1]  NA 4.1 4.5
tlag(value, 1, along_with = year)



*/
