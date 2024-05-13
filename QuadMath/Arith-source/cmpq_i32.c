#include <quadmath.h>
#include <stdint.h>

int32_t cmpq_i32(__float128 *a, int32_t b) {
	int32_t t=0;
	if((*a)<(__float128)b)  t = -1;
	if((*a)==(__float128)b) t = 0;
	if((*a)>(__float128)b)  t = 1;
	return t;
	}
