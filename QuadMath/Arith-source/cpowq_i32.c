#include <quadmath.h>
#include <stdint.h>

__complex128 cpowq_i32(__complex128 *a, int32_t b) {return cpowq(*a, (__complex128)b);}
