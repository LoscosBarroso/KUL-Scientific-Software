#ifndef tws_util_hpp
#define tws_util_hpp

#include <fstream>
#include <type_traits>
#include <cassert>
#include "vector.hpp"
#include <chrono>

namespace tws {


template<typename MV_OP>
int time_mv(MV_OP const & op, int N, int number_exp=100, int discard=5) {
  struct timespec l_start, l_end;
  double elapsed_time=0.;
  double average_time=0.;
  double squared_time=0.;
  double time_diff=0.;

  tws::vector<double> y(N,1.0);
  tws::vector<double> x(N,1.0);

  for(int exp=0;exp<number_exp+discard;exp++){
    auto t_start = std::chrono::high_resolution_clock::now();
    op(x,y);
    auto t_end = std::chrono::high_resolution_clock::now();
    if(exp>=discard){
       elapsed_time=std::chrono::duration<double>(t_end-t_start).count(); 
       time_diff=elapsed_time-average_time;
       average_time+=time_diff/(exp-discard+1);
       squared_time+=time_diff*(elapsed_time-average_time);
    }
    y(0)+=y(0);
  }
  std::cout<<N<<" "<<average_time<<" "<<std::sqrt(squared_time/(number_exp-1))<<std::endl;
  return 0 ;
}
  template<class M, typename S>
  M matrix_read(const S & filename){
     std::ifstream ifs(filename);

     if (!ifs) {
        std::cerr << "Can not open file: "<<filename<<std::endl;
        throw new std::runtime_error("Can not open given filename");
     }
     //work around to get type of provided matrix template. 
     M m1(1,1,1.0);
     decltype(m1.size()) rows=0;
     decltype(m1.size()) columns=0;


     ifs>> rows;
     ifs>> columns;
     M m(rows,columns,0);
     while(!ifs.eof()) {
        for(decltype(m1.size()) i= 0;i<rows;i++){
           for(decltype(m1.size()) j = 0;j<columns;j++){
             ifs>>m(i,j);
           }
        }
     }
     ifs.close();
     return m;
  }

  template<class V, typename S>
  V vector_read(const S & filename){
     std::ifstream ifs(filename);

     if (!ifs) {
        std::cerr << "Can not open file: "<<filename<<std::endl;
        throw new std::runtime_error("Can not open given filename");
     }
     typename V::size_type nb_elements=0;

     ifs>>nb_elements;

     V v(nb_elements,0);
     while(!ifs.eof()) {
        for(typename V::size_type i = 0;i<nb_elements;i++){
             ifs>>v(i);
        }
     }
     ifs.close();
     return v;
  }



template<typename V,typename T,typename S=double>
decltype(auto) auc_roc(V const& prediction, T const& exact, S stepsize=1e-4){
    typedef typename V::value_type value_type;
    typedef typename V::size_type size_type;
    assert(prediction.size()==exact.size()); 

    size_type N=prediction.size();
    size_type N1=std::accumulate(exact.cbegin(),exact.cend(),0);
    size_type N0=N-N1;
    value_type xiyi=0.;
    value_type xi=0.; 
    value_type yi=0;       
    for(size_type i=0;i<prediction.size();i++){
        xiyi+=prediction(i)*exact(i);
        xi+=prediction(i);
        yi+=exact(i);
    }
    
    auto corr=N*xiyi-yi*xi;
    value_type min=*std::min_element(prediction.cbegin(), prediction.cend());
    value_type max=*std::max_element(prediction.cbegin(), prediction.cend());


    V scaled_pred(prediction);
        
    if(corr>=0){
        std::transform(prediction.cbegin(), prediction.cend(),scaled_pred.begin(),[min,max](auto v){return (v-min)/(max-min);});
    } else{
        std::transform(prediction.cbegin(), prediction.cend(),scaled_pred.begin(),[min,max](auto v){return -(v-min)/(max-min)+1.0;});
    }    
   
   value_type auc_roc=0.0; 
   value_type fprate=0.0;    
   value_type rocval=0.0;  
   size_type true_positive=0;
   size_type false_positive=0;  
   for (S roc = 1.0; roc> 0.0-stepsize; roc-=stepsize){
        true_positive=0,false_positive=0;
        for(size_type i=0;i<prediction.size();i++){
           if(scaled_pred(i) > roc ){
              if(exact(i)==1){
                 true_positive++;
              }
              else{
                 false_positive++;
              }
           }
        }
        auc_roc+=(true_positive/((value_type) N1)+rocval)/2.0*(false_positive/((value_type) N0) -fprate);
        rocval=true_positive/((value_type) N1);
        fprate=false_positive/((value_type) N0);
   }//roc curve
   return auc_roc;
}

//(C) Copyright Karl Meerbergen, Gowri Suryanarayana & Joris Tavernier, 2016.
  //
  // This is code for the conjugate gradient Krylov method for solving a symmetric linear system (that is guaranteed to work for positive matrices)
  // 
  // Stopcriterion:
  //
  //   number of iterations <= maximum number of iterations max_it
  //   ||r|| <= tolerance
  //
  // Input
  //   op:
  //     binary functor:
  //         op( x, y ) computes y = A * x, i.e., op is a linear operator
  //     where x and y are Vectors.
  //
  //   r takes the right-hand side.
  //   is_vector<R> is equivalent to std::true_type
  //
  //   x takes the initial solution.
  //   is_vector<X> is equivalent to std::true_type
  //
  //   tolerance: is the residual tolerance of the linear solver, see further.
  //   maxit: is the maximum number of iterations.
  //
  // Output:
  //   r is the residual of the final solution: r = b - A * x
  //   x is the solution.
  //
  template <typename X, typename R, typename Op>
  void cg( Op const& op, X& x, R& r, double const& tolerance, int max_it ) {
    typedef typename X::value_type value_type ;
    typedef vector<typename X::value_type> container_type ;

    assert( x.size() == r.size() ) ;
    assert(tolerance>0.);
    assert(max_it>0);

    container_type  p( x.size() ) ;
    container_type  q( x.size() ) ;
    container_type  x_initial( x.size(),0. ) ;
    // Compute residual
    
    op( x, q ) ;
    r -= q ;
    auto res_norm_0 = norm_2( r ) ;
    value_type rho2 ;
    value_type rho1 ;

    for ( int it=0 ; it<max_it; ++it ) {
      if ( norm_2( r ) < tolerance*res_norm_0 ) break ;
      rho1 = inner_product( r, r ) ;

      if (it==0) {
        p = r ;
      } else {
        p = r + (rho1/rho2) * p ;
      }
      op( p, q ) ;
      value_type alpha = rho1 / inner_product( p, q ) ;
      x_initial += alpha * p ;
      r -= alpha * q ;
      rho2 = rho1 ;
    }
    x+=x_initial;
  } // cg

}
#endif
