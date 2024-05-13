#include <quadmath.h>
#include <stdint.h>

__complex128 cpowq_u32(__complex128 *a, uint32_t b) {return cpowq(*a, (__complex128)b);}
