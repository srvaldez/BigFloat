#include <quadmath.h>
#include <stdint.h>

__float128 u32_mulq(uint32_t b, __float128 *a) {return (__float128)b*(*a);}
