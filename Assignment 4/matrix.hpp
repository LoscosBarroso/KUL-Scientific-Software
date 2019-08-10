#ifndef tws_matrix_hpp
#define tws_matrix_hpp
#include "vector.hpp"
#include <cassert>
#include <iostream>
#include <cmath>
#include <typeinfo>
#include <algorithm>
#include <random>
#include <chrono>
#include <iterator>

namespace tws {

	template <typename T>
	class matrix {
		public:
				typedef T   value_type ;
				typedef int size_type;
				static_assert( std::is_arithmetic<T>::value, "Matrix: T should be arithmetic type") ;
				
		public:
				matrix( size_type rows, size_type cols )
				: rows_( rows )
				, cols_( cols )
				, size_( rows_*cols_ )
				, data_( new value_type[size_] )
				{}
				
				matrix( size_type rows, size_type cols, value_type val )
				: rows_( rows )
				, cols_( cols )
				, size_( rows_*cols_ )
				, data_( new value_type[size_] )
				{
				    std::fill_n(data_, size_, val); 
				}

				~matrix()
				{ delete [] data_ ; }
				
				matrix( matrix const& that )
				: rows_( that.rows_ )
				, cols_( that.cols_ )
				, size_( rows_*cols_ )
				, data_( new value_type[size_] )
				{
				    //calls operator=
				    (*this) = that ;
				}
				
				matrix& operator=( matrix const& that ) {
				  assert( that.rows() == rows() ) ;
					assert( that.cols() == cols() ) ;
				  std::copy( that.data_, that.data_+size_, data_ ) ;
				  return *this ;
				}

				template <typename V>
				matrix& operator=( V const& that ) {
				  assert( that.size() == size() ) ;
				    for (size_type i=0; i<rows_; ++i) {
								for (size_type j=0; j<cols_; ++j) {
				        		data_[(i*cols_)+j] = that(i,j) ;
				    		}
				    }
				    return *this ;
				}

		public:// Access
				value_type operator() ( size_type i , size_type j ) const {
				    assert( i>=0 ) ;
				    assert( i<size_ ) ;
						assert( j>=0 ) ;
				    assert( j<size_ ) ;
				    return data_[(i*cols_)+j] ;
				}
				
				value_type& operator() ( size_type i , size_type j ) {
				    assert( i>=0 ) ;
				    assert( i<size_ ) ;
						assert( j>=0 ) ;
				    assert( j<size_ ) ;
				    return data_[(i*cols_)+j] ;
				}
				
				size_type rows() const {
				    return rows_ ;
				}
				size_type cols() const {
				    return cols_ ;
				}
				size_type num_rows() const {
				    return rows_ ;
				}
				size_type num_columns() const {
				    return cols_ ;
				}
				size_type size() const {
				    return size_ ;
				}
				inline value_type* begin(){
				    return data_;
				}
				inline value_type* end(){
				    return data_+size_;
				}
				inline const value_type* cbegin() const{
				    return data_;
				}
				inline const value_type* cend() const{
				    return data_+size_;
				}
    
		public: //arithmetic operations
				 template <typename Matrix>
				 inline matrix& operator-=(Matrix const& m ) {
				    assert(this->rows()==m.rows());
						assert(this->cols()==m.cols());
				    for (size_type i=0; i<rows(); ++i) { for (size_type j=0; j<cols(); ++j) {data_[(i*cols_)+j] = data_[(i*cols_)+j]-m(i,j) ; }}
				    return *this;
				 }

				  template <typename Matrix>
				  inline matrix& operator+=(Matrix const& m ) { 
				     assert(this->rows()==m.rows());
						 assert(this->cols()==m.cols());
				     for (size_type i=0; i<rows(); ++i) { for (size_type j=0; j<cols(); ++j) {data_[(i*cols_)+j] = data_[(i*cols_)+j]+m(i,j) ; }}
				     return *this;
				  }

				  void randomize(value_type min, value_type max, int seed=1){
				     #ifdef NDEBUG
				        seed = std::chrono::system_clock::now().time_since_epoch().count();
				     #endif
				     auto engine = std::default_random_engine(seed);
				     std::uniform_real_distribution<value_type> distribution(min,max);
				     for (size_type i=0; i<rows(); ++i) { for (size_type j=0; j<cols(); ++j) {data_[(i*cols_)+j] = distribution(engine) ; }}
				  }      
				
		public: // Fortran binding:
				typedef value_type* const& pointer ;
				pointer ptr() const {
				    return data_ ;
				}
				
		private:
				size_type   rows_ ;
				size_type   cols_ ;
				size_type   size_ ;
				value_type* data_ ;
	};

	template <class T>
	struct is_matrix : public std::false_type{};


	template <class T>
	struct is_matrix<tws::matrix<T> > : public std::true_type{};

	template <typename T>
	typename std::enable_if<is_matrix<T>::value, std::ostream&>::type operator<<( std::ostream& os, T const& m ) {
		  os << "[" << m.rows() << "x" << m.cols() << "](";
		  for (typename T::size_type i=0; i< m.rows()-1; ++i) {
					for (typename T::size_type j=0; j< m.cols()-1; ++j) {
		      	os << m(i,j) << "," ;
					}
					os << m(i,m.cols()-1)<<"; " ;
		  }
			for (typename T::size_type j=0; j< m.cols()-1; ++j) {
		      os << m(m.rows()-1,j) << "," ;
			}
		      os << m(m.rows()-1,m.cols()-1)<<")" ;
		  return os ;
	}

	template <typename M>
  inline typename std::enable_if<is_matrix<M>::value, matrix<decltype(typename M::value_type())>>::type transpose( M const& v ) {
      matrix<decltype(typename M::value_type())> res(v.cols(),v.rows(),v(0,0));
      for (typename M::size_type i=0;i<v.rows();i++) {for (typename M::size_type j=0;j<v.cols();j++){res(j,i)=v(i,j);}}
      return res;
  }

  template <typename S,typename V>
  inline typename std::enable_if<(is_matrix<V>::value && std::is_arithmetic<S>::value), matrix<decltype(S()*typename V::value_type())>>::type operator*( S const& s, V const& v ) {
      matrix<decltype(S()*typename V::value_type())> res(v.rows(),v.cols(),s*v(0,0));
      for (typename V::size_type i=0;i<v.rows();i++) {for (typename V::size_type j=0;j<v.cols();j++){res(i,j)=s*v(i,j);}}
      return res;
  }

  template <typename V,typename S>
  inline typename std::enable_if<(is_matrix<V>::value && std::is_arithmetic<S>::value), matrix<decltype(S()*typename V::value_type())>>::type operator*( V const& v, S const& s ) {
      matrix<decltype(S()*typename V::value_type())> res(v.rows(),v.cols(),s*v(0,0));
      for (typename V::size_type i=0;i<v.rows();i++) {for (typename V::size_type j=0;j<v.cols();j++){res(i,j)=s*v(i,j);}}
      return res;
  }

  template <typename V1,typename V2>
  inline typename std::enable_if<(is_matrix<V1>::value && is_matrix<V2>::value), matrix<decltype(typename V1::value_type() + typename V2::value_type())>>::type operator+( V1 const& v1, V2 const& v2 ) {
      assert(v1.rows()==v2.rows());
			assert(v1.cols()==v2.cols());
      matrix<decltype(typename V1::value_type() + typename V2::value_type())> res(v1.rows(),v1.cols(),v1(0,0)+v2(0,0));
      for (typename V1::size_type i=0;i<v1.rows();i++) {for (typename V1::size_type j=0;j<v1.cols();j++){res(i,j)=v1(i,j)+v2(i,j);}}
      return res;
  }


  template <typename V1,typename V2>
  inline typename std::enable_if<(is_matrix<V1>::value && is_matrix<V2>::value), matrix<decltype(typename V1::value_type() - typename V2::value_type())>>::type operator-( V1 const& v1, V2 const& v2 ) {
			assert(v1.rows()==v2.rows());
			assert(v1.cols()==v2.cols());
      matrix<decltype(typename V1::value_type() - typename V2::value_type())> res(v1.rows(),v1.cols(),v1(0,0)-v2(0,0));
      for (typename V1::size_type i=0;i<v1.rows();i++) {for (typename V1::size_type j=0;j<v1.cols();j++){res(i,j)=v1(i,j)-v2(i,j);}}
      return res;
  }

	template<typename M, typename V>
  inline typename std::enable_if<(is_matrix<M>::value && is_vector<V>::value), vector<decltype(typename M::value_type() * typename V::value_type())>>::type operator*(M const& m, V const& v){
			assert(m.cols()==v.size());
      decltype(typename M::value_type() * typename V::value_type()) sum = 0;
      vector<decltype(typename M::value_type() * typename V::value_type())> mul(m.rows(),m(0,0)*v(0));
      for (typename M::size_type i=0;i<m.rows();i++) {
					sum = 0;
					for (typename M::size_type j=0;j<m.cols();j++) {
								sum+=m(i,j)*v(j);
					}
					mul(i)=sum;
			}
      return mul;
  }

	template<typename M, typename V>
  inline typename std::enable_if<(is_matrix<M>::value && is_vector<V>::value), vector<decltype(typename M::value_type() * typename V::value_type())>>::type operator*(V const& v,M const& m){
			assert(m.rows()==v.size());
      decltype(typename M::value_type() * typename V::value_type()) sum = 0;
      vector<decltype(typename M::value_type() * typename V::value_type())> mul(m.cols(),m(0,0)*v(0));
      for (typename M::size_type i=0;i<m.cols();i++) {
					sum = 0;
					for (typename M::size_type j=0;j<m.rows();j++) {
								sum+=m(i,j)*v(j);
					}
					mul(i)=sum;
			}
      return mul;
  }

  template<typename V1, typename V2>
  inline typename std::enable_if<(is_matrix<V1>::value && is_matrix<V2>::value), matrix<decltype(typename V1::value_type() * typename V2::value_type())>>::type operator*(V1 const& v1, V2 const& v2){
			assert(v1.cols()==v2.rows());
      decltype(typename V1::value_type() * typename V2::value_type()) sum = 0;
      matrix<decltype(typename V1::value_type() * typename V2::value_type())> mul(v1.rows(),v2.cols(),v1(0,0)*v2(0,0));
      for (typename V1::size_type i=0;i<v1.rows();i++) {
					for (typename V2::size_type j=0;j<v2.cols();j++) {
 						 sum = 0;
					   for (typename V2::size_type k=0;k<v1.cols();k++) {
								sum+=v1(i,k)*v2(k,j);
						 }
 						 mul(i,j)=sum;
					}
			}
      return mul;
  }

}

#endif
