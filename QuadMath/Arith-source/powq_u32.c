#include <quadmath.h>
#include <stdint.h>

__float128 powq_u32(__float128 *a, uint32_t b) {return powq(*a, (__float128)b);}
