#include <quadmath.h>
#include <stdint.h>

__float128 subq_u32(__float128 *a, uint32_t b) {return (*a)-(__float128)b;}
