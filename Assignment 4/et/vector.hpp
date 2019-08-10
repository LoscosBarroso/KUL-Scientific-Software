#ifndef tws_et_vector_hpp
#define tws_et_vector_hpp

#include <vector>
#include <cassert>
#include <limits>
#include <type_traits>
#include "is_vector.hpp"

namespace ET {

  template <typename T>
  class vector {
    public:
      typedef T   value_type ;
      typedef int size_type ;
      static_assert( std::is_arithmetic<T>::value, "Vector: T should be arithmetic type") ;

    public:
      vector( size_type n )
      : data_( n )
      {
#ifndef NDEBUG
        std::fill( data_.begin(), data_.end(), std::numeric_limits<value_type>::quiet_NaN() ) ;
#endif
      }

      vector( vector const& that )
      : data_( that.data_ )
      {}

      void operator=( vector const& that ) {
        assert( that.size()==size() ) ;
        std::copy( that.data_.begin(), that.data_.end(), data_.begin() ) ;
      }

      template <typename Vector>
      void operator=( Vector const& that ) {
        static_assert( is_vector<Vector>::value, "" ) ;
        assert( that.size()==size() ) ;
        for (size_type i=0; i<size(); ++i) {
          data_[i] = that(i) ;
        }
      }

    public:
      // Vector
      size_type size() const {
        return data_.size() ;
      }

      value_type operator[]( size_type i ) const {
        assert( i>=0 && i<size() ) ;
        return data_[i] ;
      }

      value_type& operator[]( size_type i ) {
        assert( i>=0 && i<size() ) ;
        return data_[i] ;
      }

      // Vector
      value_type operator()( size_type i ) const {
        assert( i>=0 && i<size() ) ;
        return data_[i] ;
      }

      value_type& operator()( size_type i ) {
        assert( i>=0 && i<size() ) ;
        return data_[i] ;
      }

    private:
      std::vector< value_type > data_ ;
  } ;

  template <typename T>
  struct is_vector< vector<T> >
  : std::true_type
  {} ;

} // namespace ET

#endif
