#include <quadmath.h>
#include <stdint.h>

__float128 double_divq(double b, __float128 *a) {return (__float128)b/(*a);}
