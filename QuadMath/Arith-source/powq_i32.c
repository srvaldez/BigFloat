#include <quadmath.h>
#include <stdint.h>

__float128 powq_i32(__float128 *a, int32_t b) {return powq(*a, (__float128)b);}
