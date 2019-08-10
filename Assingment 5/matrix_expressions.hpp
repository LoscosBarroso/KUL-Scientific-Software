#ifndef tws_matrix_expressions_hpp
#define tws_matrix_expressions_hpp
#include"matrix.hpp"
#include<type_traits>
namespace tws{
       
    template <typename M>
    class matrix_trans {
         static_assert(is_matrix<M>::value,"matrix_trans requires first argument to have succesfull is_matrix");
        public:
        typedef typename M::size_type size_type ;
        typedef decltype( typename M::value_type()) value_type ;

        public:
        matrix_trans( M const& m )
        : m_( m )
        {
            assert( m.cols()>0 && m.rows() >0) ;
        }

        size_type size() const {
            return m_.size() ;
        }

				size_type rows() const {
            return m_.cols() ;
        }
				size_type cols() const {
            return m_.rows() ;
        }

        value_type operator()( size_type i , size_type j) const {
            assert( i>=0 && j >=0) ;
            assert( j<rows() && i<cols()) ;
            return m_(j,i);
        }

        private:
        M const& m_ ;
    } ; //matrix_trans

  template <class T>
  struct is_matrix<matrix_trans<T> > {
    static bool const value = is_matrix<T>::value;
  };

    template <typename M>
    typename std::enable_if< is_matrix<M>::value, matrix_trans<M> >::type transpose(M const& m ) {
        return matrix_trans<M>(m) ;
    } //transposition operator

		template <typename V1, typename V2>
    class matrix_sum {
         static_assert(is_matrix<V1>::value,"matrix_sum requires first argument to have succesfull is_matrix");
         static_assert(is_matrix<V2>::value,"matrix_sum requires second argument to have succesfull is_matrix");
        public:
        typedef typename V1::size_type size_type ;
        typedef decltype( typename V1::value_type() + typename V2::value_type() ) value_type ;

        public:
        matrix_sum( V1 const& v1, V2 const& v2 )
        : v1_( v1 )
        , v2_( v2 )
        {
            assert( v1.cols()==v2.cols() && v1.rows() == v2.rows()) ;
        }

        size_type size() const {
            return v1_.size() ;
        }

				size_type rows() const {
            return v1_.rows() ;
        }
				size_type cols() const {
            return v1_.cols() ;
        }

        value_type operator()( size_type i , size_type j) const {
            assert( i>=0 && j >=0) ;
            assert( i<rows() && j<cols()) ;
            return v1_(i,j)+v2_(i,j);
        }

        private:
        V1 const& v1_ ;
        V2 const& v2_ ;
    } ; //matrix_sum

  template <class T,class T2>
  struct is_matrix<matrix_sum<T,T2> > {
    static bool const value = is_matrix<T>::value && is_matrix<T2>::value;
  };

    template <typename V1, typename V2>
    typename std::enable_if< is_matrix<V2>::value && is_matrix<V1>::value, matrix_sum<V1,V2> >::type operator+(V1 const& v1, V2 const& v2 ) {
        return matrix_sum<V1,V2>(v1,v2) ;
    } //operator+

    template <typename V1, typename V2>
    class matrix_minus {
        public:
         static_assert(is_matrix<V1>::value,"matrix_minus requires first argument to have succesfull is_matrix");
         static_assert(is_matrix<V2>::value,"matrix_minus requires second argument to have succesfull is_matrix");
        typedef typename V1::size_type size_type ;
        typedef decltype( typename V1::value_type() + typename V2::value_type() ) value_type ;

        public:
        matrix_minus( V1 const& v1, V2 const& v2 )
        : v1_( v1 )
        , v2_( v2 )
        {
            assert( v1.cols()==v2.cols() && v1.rows() == v2.rows()) ;
        }

        size_type size() const {
            return v1_.size() ;
        }

				size_type rows() const {
            return v1_.rows() ;
        }
				size_type cols() const {
            return v1_.cols() ;
        }
    
        value_type operator()( size_type i , size_type j) const {
            assert( i>=0 && j >=0) ;
            assert( i<rows() && j<cols()) ;
            return v1_(i,j)-v2_(i,j);
        }

        private:
        V1 const& v1_ ;
        V2 const& v2_ ;
    } ; //matrix_minus

  template <class T,class T2>
  struct is_matrix<matrix_minus<T,T2> > {
    static bool const value = is_matrix<T>::value && is_matrix<T2>::value;
  };

    template <typename V1, typename V2>
     typename std::enable_if< is_matrix<V2>::value && is_matrix<V1>::value, matrix_minus<V1,V2> >::type operator-(V1 const& v1, V2 const& v2 ) {
        return matrix_minus<V1,V2>(v1,v2) ;
    }//operator-

    template <typename S1, typename V2>
    class scalar_matrix_multiply {
         static_assert(std::is_arithmetic<S1>::value,"scalar_matrix_multiply requires first argument to have succesfull is_arithmetic");
         static_assert(is_matrix<V2>::value,"scalar_matrix_multiply requires second argument to have succesfull is_matrix");
        public:
        typedef typename V2::size_type size_type ;
        typedef decltype( S1() * typename V2::value_type() ) value_type ;

        public:
        scalar_matrix_multiply( S1 const& s1, V2 const& v2 )
        : s1_( s1 )
        , v2_( v2 )
        {
        }

        size_type size() const {
            return v2_.size() ;
        }

				size_type rows() const {
            return v2_.rows() ;
        }
				size_type cols() const {
            return v2_.cols() ;
        }

        value_type operator()( size_type i , size_type j) const {
            assert( i>=0 && j >=0) ;
            assert( i<rows() && j<cols()) ;
            return s1_*v2_(i,j);
        }


        private:
        S1 const& s1_ ;
        V2 const& v2_ ;
    } ; //scalar_matrix_multiply

  template <class S,class M>
  struct is_matrix<scalar_matrix_multiply<S,M> > {
    static bool const value = is_matrix<M>::value && std::is_arithmetic<S>::value ;
  };

    template <typename S1, typename V2>
    typename std::enable_if< is_matrix<V2>::value && std::is_arithmetic<S1>::value, scalar_matrix_multiply<S1,V2> >::type operator*(S1 const& s1, V2 const& v2 ) {
        return scalar_matrix_multiply<S1,V2>(s1,v2) ;
    }//operator*

    template <typename S1, typename V2>
    typename std::enable_if< is_matrix<V2>::value && std::is_arithmetic<S1>::value , scalar_matrix_multiply<S1,V2> >::type  operator*(V2 const& v2, S1 const& s1 ) {
        return scalar_matrix_multiply<S1,V2>(s1,v2) ;
    }//operator*

		template <typename M, typename V>
    class matrix_vector_multiply {
				//assumes column vector M*v, the resulting type is a vector
         static_assert(is_vector<V>::value,"vector_matrix_multiply requires second argument to have succesfull is_vector");
         static_assert(is_matrix<M>::value,"vector_matrix_multiply requires first argument to have succesfull is_matrix");
        public:
        typedef typename V::size_type size_type ;
        typedef decltype( typename V::value_type() * typename M::value_type() ) value_type ;

        public:
        matrix_vector_multiply( M const& m, V const& v)
        : m_( m )
        , v_( v )
        {
					assert( v.size()==m.cols()) ;
        }

        size_type size() const {
            return m_.rows() ;
        }

        value_type operator()( size_type i) const {
            assert( i>=0 ) ;
            assert( i<size());
						value_type sum = 0;
						for (typename M::size_type j=0;j<m_.cols();j++) {
									sum+=m_(i,j)*v_(j);
						}
						return sum;
        }


        private:
        M const& m_ ;
        V const& v_ ;
    } ; //matrix_vector_multiply

  template <class M,class V>
  struct is_vector<matrix_vector_multiply<M,V> > {
    static bool const value = is_matrix<M>::value && is_vector<V>::value ;
  };

    template <typename M, typename V>
    typename std::enable_if< is_matrix<M>::value && is_vector<V>::value , matrix_vector_multiply<M,V> >::type  operator*(M const& m, V const& v ) {
        return matrix_vector_multiply<M,V>(m,v) ;
    }//operator*

} // namespace tws

#endif
