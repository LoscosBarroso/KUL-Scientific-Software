#ifndef tws_et_sum_hpp
#define tws_et_sum_hpp

#include "is_vector.hpp"
#include <cassert>
#include <type_traits>

namespace ET {

  template <typename V1, typename V2>
  class vector_sum {
    public:
      typedef typename std::common_type< typename V1::value_type, typename V2::value_type>::type value_type ;
      typedef typename V1::size_type size_type ;
      static_assert( is_vector<V1>::value, "V1 should be a Vector" ) ;
      static_assert( is_vector<V2>::value, "V2 should be a Vector" ) ;

    public:
      vector_sum( V1 const& v1, V2 const& v2 ): v1_( v1 ), v2_( v2 ){assert( v1_.size()==v2_.size() ) ;}
      size_type size() const {return v1_.size() ;}
      value_type operator() (size_type i) const {assert( i>=0 && i<size() ) ;return v1_(i) + v2_(i) ;}

    private:
      V1 const& v1_ ;V2 const& v2_ ;
  } ;

  template <typename V1, typename V2>
  typename std::enable_if< is_vector<V1>::value && is_vector<V2>::value, vector_sum<V1,V2> >::type operator+( V1 const& v1, V2 const& v2 ) {
    return vector_sum<V1,V2>( v1, v2 ) ;
  }

  template <typename V1, typename V2>
  struct is_vector< vector_sum<V1,V2> >
  {static bool const value = is_vector<V1>::value && is_vector<V2>::value ;};

} // namespace ET

#endif
