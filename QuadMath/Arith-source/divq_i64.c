#include <quadmath.h>
#include <stdint.h>

__float128 divq_i64(__float128 *a, int64_t b) {return (*a)/(__float128)b;}
