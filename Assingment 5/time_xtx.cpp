#include "vector_expressions.hpp"
#include "matrix_expressions.hpp"
#include "tws_util.hpp"
template <typename TM>
class SRDAfunctor1step
{
    public:
        SRDAfunctor1step (tws::matrix<TM> m, int b) : X( tws::matrix<TM>(m.cols(),m.cols(),0))
				{X = m; beta = b;}
				template <typename TV>        
				void operator() ( tws::vector<TV>& x, tws::vector<TV>& y) const {y = transpose(X)*(X*x)+beta*x;}
    private:
				tws::matrix<TM> X;
				int beta;
};

template <typename TM>
class SRDAfunctor2steps
{
    public:
        SRDAfunctor2steps (tws::matrix<TM> m, int b) : X( tws::matrix<TM>(m.cols(),m.cols(),0))
				{X = m; beta = b;}
				template <typename TV>        
				void operator() ( tws::vector<TV>& x, tws::vector<TV>& y) const {tws::vector<TV> t(X.rows(),0.); t = X*x; y = transpose(X)*t+beta*x;}
    private:
				tws::matrix<TM> X;
				int beta;
};
	

int main(int argc, char *argv[]) {

  typedef  tws::matrix<double>  my_matrix;
  int N = 10;
  int number_exp=100;
  int discard=10;
  if(argc>1) N = std::stoi(argv[1]);
  if(argc>2) number_exp = std::stoi(argv[2]);
  if(argc>3) discard = std::stoi(argv[3]);
  my_matrix X(N,N);
  std::srand( std::time(nullptr) ) ;
  for( int j = 0 ; j < N ; ++j ) {for( int i = 0 ; i < N ; ++i ){X(i,j) = (std::rand()/ 10000000.) - 106;}}
	double beta=1e1;
	SRDAfunctor1step  xtx_op1(X,beta);
	SRDAfunctor2steps xtx_op2(X,beta);
  std::cout<<N;
  tws::time_mv(xtx_op1, N, number_exp, discard);
  tws::time_mv(xtx_op2, N, number_exp, discard);
  std::cout<<std::endl;
  return 0 ;
} 
