#include <quadmath.h>
#include <stdint.h>

__float128 i32_cmulq(int32_t b, __complex128 *a) {return (__complex128)b*(*a);}
