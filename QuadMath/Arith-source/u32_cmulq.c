#include <quadmath.h>
#include <stdint.h>

__float128 u32_cmulq(uint32_t b, __complex128 *a) {return (__complex128)b*(*a);}
