#include <dplyr.h>
using namespace dplyr ;
using namespace Rcpp ;
    
template <int RTYPE1, int RTYPE2>
class Cov : public Processor<REALSXP, Cov<RTYPE1, RTYPE2> > {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE1>::type STORAGE1 ;
        typedef typename Rcpp::traits::storage_type<RTYPE2>::type STORAGE2 ;

        Cov(SEXP x_, SEXP y_, bool narm_) : x(x_), y(y_), narm(narm_) {}
        
        double process_chunk( const SlicingIndex& indices ){
            STORAGE1 sx = 0 ;
            STORAGE2 sy = 0 ;
            double cross = 0 ;
            int count = 0 ;
            int n=indices.size() ;
            if (narm){
                for( int i=0; i<n; i++){
                    STORAGE1 currentx = x[i] ;
                    STORAGE2 currenty = y[i] ;
                    if( Rcpp::Vector<RTYPE1>::is_na(currentx) | Rcpp::Vector<RTYPE2>::is_na(currenty) ) continue ;
                    cross += currentx * currenty ;
                    sx += currentx ;
                    sy += currenty ;
                    count++ ;
                }
            }
            else{
               for( int i=0; i<n; i++){
               STORAGE1 currentx = x[i] ;
               STORAGE2 currenty = y[i] ;
               if( Rcpp::Vector<RTYPE1>::is_na(currentx) | Rcpp::Vector<RTYPE2>::is_na(currenty) ) break ;
               cross += currentx * currenty ;
               sx += currentx ;
               sy += currenty ;
               count++ ;
                }
            }
            if (count <= 1) return NA_REAL;
            return ((cross - sx*sy)/(count-1));

}

    private:
      Vector<RTYPE1> x ;
      Vector<RTYPE2> y ;
      bool narm ;
} ;

