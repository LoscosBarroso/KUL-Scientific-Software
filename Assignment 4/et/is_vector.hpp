#ifndef tws_et_is_vector_hpp
#define tws_et_is_vector_hpp

#include <type_traits>
#include <iostream>

namespace ET {

  template <typename T>
  struct is_vector
  : std::false_type
  {} ;

  template <typename T>
  typename std::enable_if< is_vector<T>::value, std::ostream&>::type operator<<( std::ostream& os, T const& v ) {
    os << "(" << v.size() << ")[" ;
    for (typename T::size_type i=0; i<v.size(); ++i) {
      os << v(i) << "," ;
    }
    os << "]" ;
    return os ;
  }

}

#endif
