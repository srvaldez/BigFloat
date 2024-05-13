#include <quadmath.h>
#include <stdint.h>

__float128 divq_i32(__float128 *a, int32_t b) {return (*a)/(__float128)b;}
