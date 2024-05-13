#include <quadmath.h>
#include <stdint.h>

__float128 i32_caddq(int32_t b, __complex128 *a) {return (__complex128)b+(*a);}
