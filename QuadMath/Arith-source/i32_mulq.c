#include <quadmath.h>
#include <stdint.h>

__float128 i32_mulq(int32_t b, __float128 *a) {return (__float128)b*(*a);}
