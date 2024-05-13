#include <quadmath.h>
#include <stdint.h>

__float128 i64_mulq(int64_t b, __float128 *a) {return (__float128)b*(*a);}
