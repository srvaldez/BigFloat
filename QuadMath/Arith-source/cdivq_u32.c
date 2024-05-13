#include <quadmath.h>
#include <stdint.h>

__float128 cdivq_u32(__complex128 *a, uint32_t b) {return (*a)/(__complex128)b;}
