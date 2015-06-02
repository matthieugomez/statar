#include <Rcpp.h>
#include <dplyr.h>
using namespace dplyr ;
using namespace Rcpp ;
/* 
template <int RTYPE>
class Tlag : public Mutater<RTYPE, Tlag<RTYPE> > {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        typedef typename Rcpp::traits::storage_type<INTSXP>::type STORAGEINT ;
        Tlag(SEXP x_, int n_, SEXP y_) : x(x_), n(n_), y(y_) {}

        void process_slice( Vector<RTYPE>& out, const SlicingIndex& index, const SlicingIndex& out_index){
            Vector<INTSXP> index2;
            Vector<INTSXP> time;
            Vector<INTSXP> time2;
            Vector<INTSXP> index3 ;
            STORAGEINT content ;
            for (int i = 0; i < index.size(); i++){
              content = y[index[i]] ;
              if (Rcpp::Vector<INTSXP>::is_na(content)){
                continue ;
              }
              index2[i] = index[i] ;
              time[i] = content - n ;
              time2[i] = content;
            }
            index3 = match(time, time2) ;
            for (int i = 0; i < index.size(); i++){
                content = index3[i] ;
                if (Rcpp::Vector<INTSXP>::is_na(content)){
                  continue ;
                }
                out[out_index[i]] = content;
              }
            }
          
    private:
      Vector<RTYPE> x ;
      int n ;
      Vector<INTSXP> y ;
} ;
*/
