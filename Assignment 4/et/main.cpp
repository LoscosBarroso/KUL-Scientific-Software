#include "vector.hpp"
#include "sum.hpp"

int main () {
  ET::vector<double> v(10) ;
  ET::vector<float> w(10) ;

  for (ET::vector<double>::size_type i=0; i<v.size(); ++i) {
    v(i) = i+1.0 ;
    w(i) = i*i+1.0 ;
  }

  std::cout << "V = " << v << std::endl ;
  std::cout << "W = " << w << std::endl ;

  ET::vector<double> z(10) ;

  z = v + w ;

  std::cout << "V+W = " << z << std::endl ;

  v = w ;

  return 0 ;
}
