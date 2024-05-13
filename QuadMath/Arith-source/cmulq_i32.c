#include <quadmath.h>
#include <stdint.h>

__float128 cmulq_i32(__complex128 *a, int32_t b) {return (*a)*(__complex128)b;}
