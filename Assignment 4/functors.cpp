#include "vector.hpp"
#include "matrix.hpp"
#include "tws_util.hpp"


class Laplacian
{
    public:
        Laplacian (int N) : _N(N){}
				template <typename V>        
				void operator() (tws::vector<V>& x, tws::vector<V>& y) const {
							assert(x.size() == y.size());
							assert(x.size() == _N); 
							y(0) =  (2*x(0))-x(1);
							for(int i = 1; i<_N-1;i++){
								y(i) =  (2*x(i))-x(i-1)-x(i+1);
							} 
							y(_N-1) =  (2*x(_N-1))-x(_N-2);
				}
    private:
				int _N;
};

template <typename T>
class Toeplitz
{
    public:
        Toeplitz (tws::vector<T> v) : _V(v),_N(v.size()){}
				template <typename VX, typename VY>        
				void operator() ( tws::vector<VX>& x, tws::vector<VY>& y) const {
							assert(x.size() == y.size());
							assert(x.size() == (_N+1)/2);
							int max = _N-1;
							int min = max - (_N-1)/2;
							int i = 0;
							while (min >=0){
								y(i) = 0;
								for(int j=min; j<=max;j++){
									y(i)+= _V(j)*x(j-min);
								}
								min--;max--;i++;
							}
				}
    private:
				tws::vector<T> _V;
				int _N;
};

template <typename T>
class Hankel
{
    public:
        Hankel (tws::vector<T> v) : _V(v),_N(v.size()){}
				template <typename VX, typename VY>        
				void operator() ( tws::vector<VX>& x, tws::vector<VY>& y) const {
							assert(x.size() == y.size());
							assert(x.size() == (_N+1)/2);
							int max = (_N-1)/2;
							int min = 0;
							int i = 0;
							while (max <=_N-1){
								y(i) = 0;
								for(int j=min; j<=max;j++){
									y(i)+= _V(j)*x(j-min);
								}
								min++;max++;i++;
							}
				}
    private:
				tws::vector<T> _V;
				int _N;
};
int main(int argc, char *argv[]) {

  typedef  tws::vector<double> my_vector;
  typedef  tws::matrix<double>  my_matrix;
  int N = 10;
  if(argc>1) N = std::stoi(argv[1]);
		my_vector out(N);
//Testing the laplacian functor, output should be
//Laplacian: [N](0,1,2,4,8,...,2^(N-3),2^(N-2)-2^(N))
	Laplacian L_op(N);
	my_vector LaplacianV(N);
	double l =-1.;
	for(int i= 0; i<N; i++){
		LaplacianV(i)=l;
		l*=2;
	}
  L_op(LaplacianV,out);
  std::cout<<"Correcnt Laplacian: [N](0,1,2,4,8,...,2^(N-3),2^(N-2)-2^(N))"<<std::endl;
  std::cout<<"Laplacian: "<<out<<std::endl;

//Testing the Toeplitz functor
	std::string data_directory("data/functors/");
  std::string s1(data_directory+"Toep_M.data");
  auto TM=tws::matrix_read<my_matrix>(s1);
	std::string s2(data_directory+"Toep_DV.data");
  auto TDV=tws::vector_read<my_vector>(s2);
	std::string s3(data_directory+"Toep_V.data");
  auto TV=tws::vector_read<my_vector>(s3);
	Toeplitz T_op(TDV);
	my_vector outT(TV.size());
	T_op(TV,outT);
	std::cout<<"Toeplitz relative error: "<<tws::norm_2(TM*TV-outT)/tws::norm_2(TV)<<std::endl;

//Testing the Hankelfunctor
 	my_matrix HM(TM);
	for(int i = 0; i<TM.rows();i++){
			for(int j = 0; j<TM.cols();j++){
					HM(i,j)=TM(i,TM.cols()-1-j);
			}
	}
	my_vector HDV(TDV);
	for(int j = 0; j<TDV.size();j++){
			HDV(j)=TDV(TDV.size()-1-j);
	}
	my_vector HV(TV);
	Hankel H_op(HDV);
	my_vector outH(HV.size());
	H_op(HV,outH);
	std::cout<<"Hankel relative error: "<<tws::norm_2(HM*HV-outH)/tws::norm_2(HV)<<std::endl;

  return 0 ;
} 
