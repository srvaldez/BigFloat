#include <quadmath.h>
#include <stdint.h>

int32_t cmpq(__float128 *a, __float128 *b) {
	int32_t t=0;
	if((*a)<(*b))  t = -1;
	if((*a)==(*b)) t = 0;
	if((*a)>(*b))  t = 1;
	return t;
	}
