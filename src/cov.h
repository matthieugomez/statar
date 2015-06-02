#include <dplyr.h>
using namespace dplyr ;
using namespace Rcpp ;
    
template <int RTYPE1, int RTYPE2,  bool NA_RM>
class Cov2 : public Processor<REALSXP, Cov2<RTYPE1, RTYPE2, NA_RM> > {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE1>::type STORAGE1 ;
        typedef typename Rcpp::traits::storage_type<RTYPE2>::type STORAGE2 ;

        Cov2(SEXP x_, SEXP y_) : x(x_), y(y_) {}
        
        double process_chunk( const SlicingIndex& indices ){
            long double sx = 0.0 ;
            long double sy = 0.0 ;
            double cross = 0.0 ;
            int n = indices.size() ;
            if (n <= 1) return NA_REAL;
             for( int i=0; i< n ; i++){
                STORAGE1 currentx = x[indices[i]] ;
                STORAGE2 currenty = y[indices[i]] ;
              if( Rcpp::Vector<RTYPE1>::is_na(currentx) | Rcpp::Vector<RTYPE2>::is_na(currenty) ) return NA_REAL ;
              sx += currentx ;
              sy += currenty ;
            }
            sx /= n ;
            sy /= n ;
            for( int i=0; i< n ; i++){
              cross += (x[indices[i]] -sx) * (y[indices[i]] - sy) ;
            }    
            return cross/(n-1);
}

    private:
      Vector<RTYPE1> x ;
      Vector<RTYPE2> y ;
} ;


template <int RTYPE1, int RTYPE2>
class Cov2<RTYPE1, RTYPE2, true> : public Processor<REALSXP, Cov2<RTYPE1, RTYPE2, true> > {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE1>::type STORAGE1 ;
        typedef typename Rcpp::traits::storage_type<RTYPE2>::type STORAGE2 ;

        Cov2(SEXP x_, SEXP y_) : x(x_), y(y_) {}
        
        double process_chunk( const SlicingIndex& indices ){
            long double sx = 0.0 ;
            long double sy = 0.0 ;
            double cross = 0.0 ;
            int count = 0 ;  
            int n = indices.size() ;
            if (n <= 1) return NA_REAL;
             for( int i=0; i< n ; i++){
              STORAGE1 currentx = x[indices[i]] ;
              STORAGE2 currenty = y[indices[i]] ;
              if( Rcpp::Vector<RTYPE1>::is_na(currentx) | Rcpp::Vector<RTYPE2>::is_na(currenty) ) continue;
              sx += currentx ;
              sy += currenty ;
              count++ ;
            }
            sx /= count ;
            sy /= count ;
            if (count <= 1) return NA_REAL;
            for( int i=0; i < n ; i++){
                STORAGE1 currentx = x[indices[i]] ;
                STORAGE2 currenty = y[indices[i]] ;
                if( Rcpp::Vector<RTYPE1>::is_na(currentx) | Rcpp::Vector<RTYPE2>::is_na(currenty) ) continue ;
                cross += (currentx -sx)* (currenty - sy) ;
            }
            return cross /(count-1);
}

    private:
      Vector<RTYPE1> x ;
      Vector<RTYPE2> y ;
} ;