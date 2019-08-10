#include "vector_expressions.hpp"
#include "matrix_expressions.hpp"
#include "tws_util.hpp"
/*
The vector expressions are needed for CG. 

You will not need the addition, substraction and scalar operators for the matrices. 

Adjust the file names or uncomment these matrix operations to avoid ambiguity (if there is any)

*/
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

  typedef  tws::vector<double> my_vector;
  typedef  tws::matrix<double>  my_matrix;

  std::string data_directory("data/gaussian/");
  if(argc>1) data_directory = argv[1];
  
  std::cout<<"Testing the single expression method: "<<std::endl;
  //madelon: /home/jorist/Codes/TWS/TWS17/C++3/Solution_code/madelon/
  //gauss: /home/jorist/Codes/TWS/TWS17/C++3/Solution_code/gaussian/
  std::string s1(data_directory+"train.data");
  auto X=tws::matrix_read<my_matrix>(s1);
  std::string s2(data_directory+"train.labels");
  auto labels=tws::vector_read<my_vector>(s2);
  std::string s3(data_directory+"valid.data");
  auto Xtest=tws::matrix_read<my_matrix>(s3);
  std::string s4(data_directory+"valid.labels");
  auto test_labels=tws::vector_read<tws::vector<int>>(s4);

  my_vector x(X.num_columns(),0.) ; 
  my_vector b(X.num_columns(),0.) ; 
  my_vector b_ex(X.num_columns(),0.) ; 
  b=transpose(X)*labels;
  b_ex=b;

  double beta=1e1;

	SRDAfunctor1step xtx_op(X,beta);
  auto t_start = std::chrono::high_resolution_clock::now();
  tws::cg( xtx_op, x, b, 1.e-10, X.num_columns()*X.num_rows() ) ;

  xtx_op ( x, b) ;
  auto t_end = std::chrono::high_resolution_clock::now();
  std::cout<<"relative error: "<<tws::norm_2(b-b_ex)/tws::norm_2(b_ex)<<std::endl;

  my_vector train_rating(X.num_rows(),1) ; 
  train_rating=X*x;
  std::transform(labels.begin(),labels.end(),labels.begin(),[](auto v){return (v+1)/2;}); 
  std::cout<<"train auc roc: "<<tws::auc_roc(train_rating,labels)<<std::endl;    

  my_vector test_rating(Xtest.num_rows(),1) ; 
  test_rating=Xtest*x;
  std::transform(test_labels.begin(),test_labels.end(),test_labels.begin(),[](auto v){return (v+1)/2;}); 
  std::cout<<"test auc roc: "<<tws::auc_roc(test_rating,test_labels)<<std::endl;

  std::cout<< "elapsed time: " << std::chrono::duration<double>(t_end-t_start).count() << "s." <<std::endl<< std::endl;

  std::cout<<"Testing the two expression method: "<<std::endl;
	X=tws::matrix_read<my_matrix>(s1);
  labels=tws::vector_read<my_vector>(s2);
  Xtest=tws::matrix_read<my_matrix>(s3);
  test_labels=tws::vector_read<tws::vector<int>>(s4);

  b=transpose(X)*labels;
  b_ex=b;

	SRDAfunctor2steps xtx_op2(X,beta);
  t_start = std::chrono::high_resolution_clock::now();
  tws::cg( xtx_op2, x, b, 1.e-10, X.num_columns()*X.num_rows() ) ;

  xtx_op2 ( x, b) ;
  t_end = std::chrono::high_resolution_clock::now();
  std::cout<<"relative error: "<<tws::norm_2(b-b_ex)/tws::norm_2(b_ex)<<std::endl;

  train_rating=X*x;
  std::transform(labels.begin(),labels.end(),labels.begin(),[](auto v){return (v+1)/2;}); 
  std::cout<<"train auc roc: "<<tws::auc_roc(train_rating,labels)<<std::endl;    

  test_rating=Xtest*x;
  std::transform(test_labels.begin(),test_labels.end(),test_labels.begin(),[](auto v){return (v+1)/2;}); 
  std::cout<<"test auc roc: "<<tws::auc_roc(test_rating,test_labels)<<std::endl;

  std::cout<< "elapsed time: " << std::chrono::duration<double>(t_end-t_start).count() << "s." << std::endl<< std::endl;
  return 0 ;
} 
