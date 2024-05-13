#include <quadmath.h>
#include <stdint.h>

__float128 double_mulq(double b, __float128 *a) {return (__float128)b*(*a);}
