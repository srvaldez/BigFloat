#include <quadmath.h>
#include <stdint.h>

int32_t cmpq_u32(__float128 *a, uint32_t b) {
	int32_t t=0;
	if((*a)<(__float128)b)  t = -1;
	if((*a)==(__float128)b) t = 0;
	if((*a)>(__float128)b)  t = 1;
	return t;
	}
