#include <quadmath.h>
#include <stdint.h>

__float128 divq_double(__float128 *a, double b) {return (*a)/(__float128)b;}
