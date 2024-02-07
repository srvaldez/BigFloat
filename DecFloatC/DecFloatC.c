
#define _CRT_SECURE_NO_WARNINGS // Visual C
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#include <limits.h>
#include <string.h>
#include <ctype.h>
#include <sys/time.h>

int NUMBER_OF_DIGITS=0;
int NUM_DIGITS=0;
int NUM_DIGITS2=0;
int NUM_DWORDS=0;
int NUM_BYTES=0;
int EXTRAWORDS = 0;

const int32_t BIAS = 1073741824; //2 ^ 30

static const long long p10_constants[19] = {
	1ll,
	10ll,
	100ll,
	1000ll,
	10000ll,
	100000ll,
	1000000ll,
	10000000ll,
	100000000ll,
	1000000000ll,
	10000000000ll,
	100000000000ll,
	1000000000000ll,
	10000000000000ll,
	100000000000000ll,
	1000000000000000ll,
	10000000000000000ll,
	100000000000000000ll,
	1000000000000000000ll
};

// error definitions

#define DIVZ_ERR        1;                    //divide by zero
#define EXPO_ERR        2;                    //exponent overflow error
#define EXPU_ERR        3;                    //exponent underflow error

typedef struct {
	int32_t sign;
	uint32_t exponent;
	uint32_t* mantissa;
}
DecFloat_struct;

DecFloat_struct pi_df, pi2_df, pi_half_df, ln2_df, ln10_df, Log10e_df, exp1_df;
void fpadd(DecFloat_struct* result, DecFloat_struct* x, DecFloat_struct* y, int32_t dwords);
void fpsub(DecFloat_struct* result, DecFloat_struct* x, DecFloat_struct* y, int32_t dwords);
void printdf(DecFloat_struct* z, int32_t dwords);
void dbl2fp(DecFloat_struct* result, double x, int32_t dwords);
void ldbl2fp(DecFloat_struct* result, long double x, int32_t dwords);
void fpdiv_si(DecFloat_struct* result, DecFloat_struct* num, int32_t den, int32_t dwords);
void fpexp_aux(DecFloat_struct* result, DecFloat_struct* x, int32_t dwords);
void si2fp(DecFloat_struct* result, int64_t m, int32_t dwords);
int32_t fpcmp(DecFloat_struct* x, DecFloat_struct* y, int32_t dwords);

double my_timer() {
	clock_t ticks;
	double et;
	ticks = clock();
	et = (double)ticks / CLOCKS_PER_SEC;
	return et;
}
/*
double my_timer() {
	struct timeval  tv1;
	gettimeofday(&tv1, NULL);
	double et;
	et = (double)(tv1.tv_usec)/1000000;
	return et;
}
*/

void DecFloat_init(DecFloat_struct* df, int32_t digits) {
	int32_t  n;
	if (NUMBER_OF_DIGITS==0) {
		if (digits==0) {
			digits=16;
		}
		NUMBER_OF_DIGITS=digits;
	}
	n = digits / 8;
	if ((8 * n) == digits) {
		NUMBER_OF_DIGITS = digits;
	}
	else {
		NUMBER_OF_DIGITS = 8 * (n + 1);
	}
	NUM_DIGITS = NUMBER_OF_DIGITS;
	NUM_DWORDS = NUMBER_OF_DIGITS / 8;
	NUM_BYTES = 4 * (NUM_DWORDS);
	
	df->sign = 0;
	df->exponent = 0;
	df->mantissa = calloc((NUM_DWORDS + 1 + EXTRAWORDS), 4);
	if (df->mantissa == NULL) {
		printf("Error! memory not allocated.");
		exit(4);
	}
}

void DecFloat_clear(DecFloat_struct* df) {
	free(df->mantissa);
	df->mantissa = 0;
}

void DecFloat_eps(DecFloat_struct* z, int32_t digits) {
	si2fp(z, 1, NUM_DWORDS + 1);
	z->exponent=(uint32_t)(-(digits)+BIAS + 1);
	z->mantissa[0] = 10000000ul;
}

void copydf(DecFloat_struct* result, DecFloat_struct* source, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	int32_t i;
	result->sign = source->sign;
	result->exponent = source->exponent;
	for (i = 0; i <= dwords; i++)
		result->mantissa[i] = source->mantissa[i];
}

void fpnegcopy(DecFloat_struct* result, DecFloat_struct* x, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	copydf(result, x, dwords);
	result->sign ^= (-1);
}

void fpneg(DecFloat_struct* result) {
	result->sign ^= (-1);
}

void swapdf(DecFloat_struct* x, DecFloat_struct* y) {
	int32_t s;
	uint32_t ex;
	uint32_t* m;
	s = x->sign;
	x->sign = y->sign;
	y->sign = s;
	ex = x->exponent;
	x->exponent = y->exponent;
	y->exponent = ex;
	m = x->mantissa;
	x->mantissa = y->mantissa;
	y->mantissa = m;
}

int log10_32(uint32_t value) {
	if (value >= 1000000000ul) return 9;
	if (value >= 100000000ul) return 8;
	if (value >= 10000000ul) return 7;
	if (value >= 1000000ul) return 6;
	if (value >= 100000ul) return 5;
	if (value >= 10000ul) return 4;
	if (value >= 1000ul) return 3;
	if (value >= 100ul) return 2;
	if (value >= 10ul) return 1;
	if (value >= 1ul) return 0;
	return -1;
}

int log10_64(uint64_t value) {
	if (value >= 10000000000000000000ull) return 19;
	if (value >= 1000000000000000000ull) return 18;
	if (value >= 100000000000000000ull) return 17;
	if (value >= 10000000000000000ull) return 16;
	if (value >= 1000000000000000ull) return 15;
	if (value >= 100000000000000ull) return 14;
	if (value >= 10000000000000ull) return 13;
	if (value >= 1000000000000ull) return 12;
	if (value >= 100000000000ull) return 11;
	if (value >= 10000000000ull) return 10;
	if (value >= 1000000000ull) return 9;
	if (value >= 100000000ull) return 8;
	if (value >= 10000000ull) return 7;
	if (value >= 1000000ull) return 6;
	if (value >= 100000ull) return 5;
	if (value >= 10000ull) return 4;
	if (value >= 1000ull) return 3;
	if (value >= 100ull) return 2;
	if (value >= 10ull) return 1;
	if (value >= 1ull) return 0;
	return -1;
}

double ipower(double x, int32_t e) {
	//take x to an integer power
	double z, y;
	int32_t n;
	y = x;
	n = abs(e);
	z = 1;
	while (n > 0) {
		while ((n & 1) == 0) {
			n = n / 2;
			y = y * y;
		}
		n = n - 1;
		z = y * z;
	}
	if (e < 0) z = 1 / z;
	return z;
}

long double ipowerl(long double x, int32_t e) {
	//take x to an integer power
	long double z, y;
	int32_t n;
	y = x;
	n = abs(e);
	z = 1L;
	while (n > 0) {
		while ((n & 1) == 0) {
			n = n / 2;
			y = y * y;
		}
		n = n - 1;
		z = y * z;
	}
	if (e < 0) z = 1L / z;
	return z;
}

int32_t fpfix_is_odd(DecFloat_struct* num) {
	int32_t ex, ex2, j, k;

	ex = (num->exponent & 0x7fffffff) - BIAS;
	if (ex < 1) return 0;
	if (ex >= NUM_DIGITS) return -1;
	ex2 = ex / 8;
	k = ex2;
	j = ex % 8;

	if (j == 1) return ((num->mantissa[k] / 10000000) & 1);
	if (j == 2) return ((num->mantissa[k] / 1000000) & 1);
	if (j == 3) return ((num->mantissa[k] / 100000) & 1);
	if (j == 4) return ((num->mantissa[k] / 10000) & 1);
	if (j == 5) return ((num->mantissa[k] / 1000) & 1);
	if (j == 6) return ((num->mantissa[k] / 100) & 1);
	if (j == 7) return ((num->mantissa[k] / 10) & 1);
	if (j == 8) return (num->mantissa[k] & 1);

	return 0;
}

//integer part of num
void fpfix(DecFloat_struct* result, DecFloat_struct* num, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;
	DecFloat_struct ip;
	int32_t ex, ex2, j, k;

	DecFloat_init(&ip, NUM_DIGITS);

	ex = (num->exponent & 0x7FFFFFFF) - BIAS;
	if (ex < 1) {
		copydf(result, &ip, dwords);
		DecFloat_clear(&ip);
		return;
	}
	if (ex >= (8 * dwords)) {
		copydf(result, num, dwords);
		DecFloat_clear(&ip);
		return;
	}
	ex2 = ex / 8;
	k = ex2;
	j = ex % 8;
	while (ex2 > 0) {
		ex2 -= 1;
		ip.mantissa[ex2] = num->mantissa[ex2];
	}
	if (j == 1) {
		ip.mantissa[k] = 10000000 * (num->mantissa[k] / 10000000);
	}
	else if (j == 2) {
		ip.mantissa[k] = 1000000 * (num->mantissa[k] / 1000000);
	}
	else if (j == 3) {
		ip.mantissa[k] = 100000 * (num->mantissa[k] / 100000);
	}
	else if (j == 4) {
		ip.mantissa[k] = 10000 * (num->mantissa[k] / 10000);
	}
	else if (j == 5) {
		ip.mantissa[k] = 1000 * (num->mantissa[k] / 1000);
	}
	else if (j == 6) {
		ip.mantissa[k] = 100 * (num->mantissa[k] / 100);
	}
	else if (j == 7) {
		ip.mantissa[k] = 10 * (num->mantissa[k] / 10);
	}
	else if (j == 8) {
		ip.mantissa[k] = num->mantissa[k];
	}
	ip.exponent = ex + BIAS;
	ip.sign = num->sign;
	copydf(result, &ip, dwords);
	DecFloat_clear(&ip);
}

//fractional part of num
void fpfrac(DecFloat_struct* result, DecFloat_struct* num, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;
	DecFloat_struct n;
	DecFloat_init(&n, NUM_DIGITS);
	fpfix(&n, num, dwords);
	fpsub(result, num, &n, dwords);
	DecFloat_clear(&n);
}

double fp2dbl(DecFloat_struct* n) {
	double dbl, tp;
	int32_t ex, sign;
	if (n->sign != 0) {
		sign = -1;
	}
	else {
		sign = 1;
	}
	if (n->exponent > 0) {
		ex = (n->exponent & 0x7fffffff) - BIAS - 1;
	}
	else {
		ex = 0;
	}
	if (ex > 308) return INFINITY;
	if (ex < (-308)) return 0;
	dbl = n->mantissa[0] + (n->mantissa[1] / 100000000.0);
	dbl = dbl / 10000000;
	tp = ipower(10.0, ex);
	return dbl * tp * sign;
}

long double fp2ldbl(DecFloat_struct* n) {
	long double ldbl, tp;
	int32_t ex, sign;
	if (n->sign != 0) {
		sign = -1;
	}
	else {
		sign = 1;
	}
	if (n->exponent > 0) {
		ex = (n->exponent & 0x7fffffff) - BIAS - 1;
	}
	else {
		ex = 0;
	}
	if (ex > 4932) return INFINITY;
	if (ex < (-4932)) return 0;
	tp=100000000.0L;
	ldbl = (long double)n->mantissa[0]*tp;
	ldbl = (ldbl + (long double)n->mantissa[1]);
	ldbl = ldbl + (long double)n->mantissa[2]/tp;
	ldbl /= 1000000000000000.0L;
	tp = ipowerl(10.0L, ex);
	return ldbl * tp * sign;
}

int32_t fp2i(DecFloat_struct* x) {
	int32_t e, ex, n;
	DecFloat_struct tmp;
	DecFloat_init(&tmp, NUM_DIGITS);
	si2fp(&tmp, 2147483647, 2);
	n=x->sign;
	x->sign=0;
	if (fpcmp(x, &tmp, 3)>0) {
		x->sign=n;
		DecFloat_clear(&tmp);
		return 2147483647;
	}
	
	if (x->exponent != 0 ) {
		ex=(x->exponent & 0x7FFFFFFFul)-BIAS-1;
	}
	else {
		ex=0;
	}
	e=abs(ex);
	si2fp(&tmp, 5, 3);
	tmp.exponent-=1;
	fpadd(&tmp, &tmp, x, 3);
	fpfix(&tmp, &tmp, 2);
	x->sign=n;
	if (e>7) {
		n=tmp.mantissa[0]*p10_constants[e-7]+(tmp.mantissa[1]/p10_constants[15-e]);
	}
	else {
		n=tmp.mantissa[0]/p10_constants[7-e];
	}
	
	DecFloat_clear(&tmp);
	if (x->sign!=0) n=-n;
	return n;
}

uint32_t fp2ui(DecFloat_struct* x) {
	int32_t e, ex;
	uint32_t m;
	DecFloat_struct tmp;
	DecFloat_init(&tmp, NUM_DIGITS);

	if (fpcmp(x, &tmp, 3)<0) {
		DecFloat_clear(&tmp);
		return 0;
	}
	si2fp(&tmp, 4294967295, 2);
	if (fpcmp(x, &tmp, 3)>0) {
		DecFloat_clear(&tmp);
		return 4294967295;
	}
	if (x->exponent != 0 ) {
		ex=(x->exponent & 0x7FFFFFFFul)-BIAS-1;
	}
	else {
		ex=0;
	}
	e=abs(ex);
	si2fp(&tmp, 5, 3);
	tmp.exponent-=1;
	fpadd(&tmp, &tmp, x, 3);
	fpfix(&tmp, &tmp, 2);
	if (e>7) {
		m=tmp.mantissa[0]*p10_constants[e-7]+(tmp.mantissa[1]/p10_constants[15-e]);
	}
	else {
		m=tmp.mantissa[0]/p10_constants[7-e];
	}
	
	DecFloat_clear(&tmp);
	return m;
}

int64_t fp2i64(DecFloat_struct* x) {
	int32_t e, ex, n;
	int64_t m;
	DecFloat_struct tmp;
	DecFloat_init(&tmp, NUM_DIGITS);
	si2fp(&tmp, 9223372036854775807, 3);
	n=x->sign;
	x->sign=0;
	if (fpcmp(x, &tmp, 3)>0) {
		x->sign=n;
		DecFloat_clear(&tmp);
		return 9223372036854775807;
	}
	
	if (x->exponent != 0 ) {
		ex=(x->exponent & 0x7FFFFFFFul)-BIAS-1;
	}
	else {
		ex=0;
	}
	e=abs(ex);
	si2fp(&tmp, 5, 3);
	tmp.exponent-=1;
	fpadd(&tmp, &tmp, x, 3);
	fpfix(&tmp, &tmp, 3);
	x->sign=n;
	if (e>15) {
		m=(tmp.mantissa[0]*100000000ll+tmp.mantissa[1])*p10_constants[e-15]+(tmp.mantissa[2]/p10_constants[23-e]);
	}
	else if (e>7) {
		m=tmp.mantissa[0]*p10_constants[e-7]+tmp.mantissa[1]/p10_constants[15-e];
	}
	else {
		m=tmp.mantissa[0]/p10_constants[7-e];
	}
	DecFloat_clear(&tmp);
	if (x->sign!=0) m=-m;
	return m;
}

uint64_t fp2ui64(DecFloat_struct* x) {
	int32_t e, ex;
	uint64_t m;
	DecFloat_struct tmp;
	DecFloat_init(&tmp, NUM_DIGITS);

	if (fpcmp(x, &tmp, 3)<0) {
		DecFloat_clear(&tmp);
		return 0ull;
	}
	
	tmp.mantissa[0]=18446744;
	tmp.mantissa[1]=7370955;
	tmp.mantissa[2]=16150000;
	tmp.exponent=19+BIAS+1;

	if (fpcmp(x, &tmp, 3)>0) {
		DecFloat_clear(&tmp);
		return 18446744073709551615ull;
	}	
	if (x->exponent != 0 ) {
		ex=(x->exponent & 0x7FFFFFFFul)-BIAS-1;
	}
	else {
		ex=0;
	}
	e=abs(ex);
	si2fp(&tmp, 5, 3);
	tmp.exponent-=1;
	fpadd(&tmp, &tmp, x, 3);
	fpfix(&tmp, &tmp, 3);

	if (e>15) {
		m=(tmp.mantissa[0]*100000000ll+tmp.mantissa[1])*p10_constants[e-15]+(tmp.mantissa[2]/p10_constants[23-e]);
	}
	else if (e>7) {
		m=tmp.mantissa[0]*p10_constants[e-7]+tmp.mantissa[1]/p10_constants[15-e];
	}
	else {
		m=tmp.mantissa[0]/p10_constants[7-e];
	}
	DecFloat_clear(&tmp);
	return m;
}

void si2fp(DecFloat_struct* result, int64_t m, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;
	int32_t ex, i;
	int64_t mm = llabs(m);

	for (i = 0; i <= dwords; i++) {
		result->mantissa[i] = 0;
	}
	if (m == 0) {
		result->exponent = 0;
		result->sign = 0;
		return;
	}
	ex = (int32_t)(log10_64(mm));
	result->exponent = BIAS + ex + 1;

	if (ex > 15) {
		ex -= 7;
		result->mantissa[0] = (int32_t)(mm / p10_constants[ex]);
		mm = mm % p10_constants[ex];
		ex -= 8;
		result->mantissa[1] = (int32_t)(mm / p10_constants[ex]);
		mm = mm % p10_constants[ex];
		result->mantissa[2] = (int32_t)(mm * p10_constants[8 - ex]);
	}
	else if (ex >= 8) {
		ex -= 7;
		result->mantissa[0] = (int32_t)(mm / p10_constants[ex]);
		mm = mm % p10_constants[ex];
		result->mantissa[1] = (int32_t)(mm * p10_constants[8 - ex]);
	}
	else if (ex < 8) {
		result->mantissa[0] = (int32_t)(mm * p10_constants[7 - ex]);
	}
	if (m < 0) {
		result->sign = -1;
	}
	else {
		result->sign = 0;
	}
}

void ui2fp(DecFloat_struct* result, uint64_t mm, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;
	int32_t ex, i;

	for (i = 0; i <= dwords; i++) {
		result->mantissa[i] = 0;
	}
	if (mm == 0) {
		result->exponent = 0;
		result->sign = 0;
		return;
	}
	ex = (int32_t)(log10_64(mm));
	result->exponent = BIAS + ex + 1;

	if (ex > 15) {
		ex -= 7;
		result->mantissa[0] = (int32_t)(mm / p10_constants[ex]);
		mm = mm % p10_constants[ex];
		ex -= 8;
		result->mantissa[1] = (int32_t)(mm / p10_constants[ex]);
		mm = mm % p10_constants[ex];
		result->mantissa[2] = (int32_t)(mm * p10_constants[8 - ex]);
	}
	else if (ex >= 8) {
		ex -= 7;
		result->mantissa[0] = (int32_t)(mm / p10_constants[ex]);
		mm = mm % p10_constants[ex];
		result->mantissa[1] = (int32_t)(mm * p10_constants[8 - ex]);
	}
	else if (ex < 8) {
		result->mantissa[0] = (int32_t)(mm * p10_constants[7 - ex]);
	}
	result->sign = 0;
}

void rshift_1(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	uint32_t v1, v2;
	for (int32_t i = dwords; i > 0; i--) {
		v1 = mantissa->mantissa[i] / 10;
		v2 = mantissa->mantissa[i - 1] % 10;
		v2 = v2 * 10000000 + v1;
		mantissa->mantissa[i] = v2;
	}
	mantissa->mantissa[0] = mantissa->mantissa[0] / 10;
}

void lshift_1(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	uint32_t v1, v2;
	for (int32_t i = 0; i <= (dwords - 1); i++) {
		v1 = mantissa->mantissa[i] % 10000000;
		v2 = mantissa->mantissa[i + 1] / 10000000;
		mantissa->mantissa[i] = v1 * 10 + v2;
		mantissa->mantissa[i + 1] = mantissa->mantissa[i + 1] % 10000000;
	}
	mantissa->mantissa[dwords] = 10 * (mantissa->mantissa[dwords] % 10000000);
}

void rshift_2(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	uint32_t v1, v2;
	for (int32_t i = dwords; i > 0; i--) {
		v1 = mantissa->mantissa[i] / 100;
		v2 = mantissa->mantissa[i - 1] % 100;
		v2 = v2 * 1000000 + v1;
		mantissa->mantissa[i] = v2;
	}
	mantissa->mantissa[0] = mantissa->mantissa[0] / 100;
}

void lshift_2(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	uint32_t v1, v2;
	for (int32_t i = 0; i <= (dwords - 1); i++) {
		v1 = mantissa->mantissa[i] % 1000000;
		v2 = mantissa->mantissa[i + 1] / 1000000;
		mantissa->mantissa[i] = v1 * 100 + v2;
		mantissa->mantissa[i + 1] = mantissa->mantissa[i + 1] % 1000000;
	}
	mantissa->mantissa[dwords] = 100 * (mantissa->mantissa[dwords] % 1000000);
}

void rshift_3(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	uint32_t v1, v2;
	for (int32_t i = dwords; i > 0; i--) {
		v1 = mantissa->mantissa[i] / 1000;
		v2 = mantissa->mantissa[i - 1] % 1000;
		v2 = v2 * 100000 + v1;
		mantissa->mantissa[i] = v2;
	}
	mantissa->mantissa[0] = mantissa->mantissa[0] / 1000;
}

void lshift_3(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	uint32_t v1, v2;
	for (int32_t i = 0; i <= (dwords - 1); i++) {
		v1 = mantissa->mantissa[i] % 100000;
		v2 = mantissa->mantissa[i + 1] / 100000;
		mantissa->mantissa[i] = v1 * 1000 + v2;
		mantissa->mantissa[i + 1] = mantissa->mantissa[i + 1] % 100000;
	}
	mantissa->mantissa[dwords] = 1000 * (mantissa->mantissa[dwords] % 100000);
}

void rshift_4(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	uint32_t v1, v2;
	for (int32_t i = dwords; i > 0; i--) {
		v1 = mantissa->mantissa[i] / 10000;
		v2 = mantissa->mantissa[i - 1] % 10000;
		v2 = v2 * 10000 + v1;
		mantissa->mantissa[i] = v2;
	}
	mantissa->mantissa[0] = mantissa->mantissa[0] / 10000;
}

void lshift_4(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	uint32_t v1, v2;
	for (int32_t i = 0; i <= (dwords - 1); i++) {
		v1 = mantissa->mantissa[i] % 10000;
		v2 = mantissa->mantissa[i + 1] / 10000;
		mantissa->mantissa[i] = v1 * 10000 + v2;
		mantissa->mantissa[i + 1] = mantissa->mantissa[i + 1] % 10000;
	}
	mantissa->mantissa[dwords] = 10000 * (mantissa->mantissa[dwords] % 10000);
}

void rshift_5(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	uint32_t v1, v2;
	for (int32_t i = dwords; i > 0; i--) {
		v1 = mantissa->mantissa[i] / 100000;
		v2 = mantissa->mantissa[i - 1] % 100000;
		v2 = v2 * 1000 + v1;
		mantissa->mantissa[i] = v2;
	}
	mantissa->mantissa[0] = mantissa->mantissa[0] / 100000;
}

void lshift_5(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	uint32_t v1, v2;
	for (int32_t i = 0; i <= (dwords - 1); i++) {
		v1 = mantissa->mantissa[i] % 1000;
		v2 = mantissa->mantissa[i + 1] / 1000;
		mantissa->mantissa[i] = v1 * 100000 + v2;
		mantissa->mantissa[i + 1] = mantissa->mantissa[i + 1] % 1000;
	}
	mantissa->mantissa[dwords] = 100000 * (mantissa->mantissa[dwords] % 1000);
}

void rshift_6(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	uint32_t v1, v2;
	for (int32_t i = dwords; i > 0; i--) {
		v1 = mantissa->mantissa[i] / 1000000;
		v2 = mantissa->mantissa[i - 1] % 1000000;
		v2 = v2 * 100 + v1;
		mantissa->mantissa[i] = v2;
	}
	mantissa->mantissa[0] = mantissa->mantissa[0] / 1000000;
}

void lshift_6(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	uint32_t v1, v2;
	for (int32_t i = 0; i <= (dwords - 1); i++) {
		v1 = mantissa->mantissa[i] % 100;
		v2 = mantissa->mantissa[i + 1] / 100;
		mantissa->mantissa[i] = v1 * 1000000 + v2;
		mantissa->mantissa[i + 1] = mantissa->mantissa[i + 1] % 100;
	}
	mantissa->mantissa[dwords] = 1000000 * (mantissa->mantissa[dwords] % 100);
}

void rshift_7(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	uint32_t v1, v2;
	for (int32_t i = dwords; i > 0; i--) {
		v1 = mantissa->mantissa[i] / 10000000;
		v2 = mantissa->mantissa[i - 1] % 10000000;
		v2 = v2 * 10 + v1;
		mantissa->mantissa[i] = v2;
	}
	mantissa->mantissa[0] = mantissa->mantissa[0] / 10000000;
}

void lshift_7(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	uint32_t v1, v2;
	for (int32_t i = 0; i <= (dwords - 1); i++) {
		v1 = mantissa->mantissa[i] % 10;
		v2 = mantissa->mantissa[i + 1] / 10;
		mantissa->mantissa[i] = v1 * 10000000 + v2;
		mantissa->mantissa[i + 1] = mantissa->mantissa[i + 1] % 10;
	}
	mantissa->mantissa[dwords] = 10000000 * (mantissa->mantissa[dwords] % 10);
}

void rshift_8(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	for (int32_t i = dwords; i > 0; i--) {
		mantissa->mantissa[i] = mantissa->mantissa[i - 1];
	}
	mantissa->mantissa[0] = 0;
}

void lshift_8(DecFloat_struct* mantissa, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	for (int32_t i = 0; i <= (dwords - 1); i++) {
		mantissa->mantissa[i] = mantissa->mantissa[i + 1];
	}
	mantissa->mantissa[dwords] = 0;
}

int32_t fpcmp(DecFloat_struct* x, DecFloat_struct* y, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;
	int32_t i;
	int64_t c;
	if (x->sign < y->sign) {
		return -1;
	}
	if (x->sign > y->sign) {
		return 1;
	}
	if (x->exponent < y->exponent) {
		if (x->sign == 0) {
			return -1;
		}
		else {
			return 1;
		}
	}
	if (x->exponent > y->exponent) {
		if (x->sign == 0) {
			return 1;
		}
		else {
			return -1;
		}
	}

	for (i = 0; i <= dwords; i++) {
		c = (int64_t)(x->mantissa[i]) - (int64_t)(y->mantissa[i]);
		if (c != 0) break;
	}
	if (c == 0) return 0;
	if (c < 0) {
		if (x->sign == 0) {
			return -1;
		}
		else {
			return 1;
		}
	}
	if (c > 0) {
		if (x->sign == 0) {
			return 1;
		}
		else {
			return -1;
		}
	}
	return 0;
}

int32_t fpcmp_abs(DecFloat_struct* x, DecFloat_struct* y, int32_t dwords) {
	dwords += 1;
	if (dwords > NUM_DWORDS + 1) dwords = NUM_DWORDS + 1;
	int32_t xs, ys, t;
	xs=x->sign;
	ys=y->sign;
	x->sign=0;
	y->sign=0;
	t=fpcmp(x, y, dwords);
	x->sign=xs;
	y->sign=ys;
	return t;
}

int32_t norm_fac1(DecFloat_struct* fac1, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;
	// normalize the number in fac1
	// all routines exit through this one.

	//see if the mantissa is all zeros.
	//if so, set the exponent and sign equal to 0.
	int32_t i, f = 0;
	for (i = 0; i <= dwords; i++) {
		if (fac1->mantissa[i] > 0) f = 1;
	}
	if (f == 0) {
		fac1->exponent = 0;
		fac1->sign = 0;
		return 0;
		//if the highmost digit in fac1_man is nonzero,
		//shift the mantissa right 1 digit and
		//increment the exponent
	}
	if (fac1->mantissa[0] > 99999999) {
		rshift_1(fac1, dwords);
		fac1->exponent += 1;
	}
	else {
		//now shift fac1_man 1 to the left until a
		//nonzero digit appears in the }-to-highest
		//digit of fac1_man.  decrement exponent for
		//each shift.
		while (fac1->mantissa[0] == 0) {
			lshift_8(fac1, dwords);
			fac1->exponent -= 8;
			if (fac1->exponent == 0) {
				return EXPU_ERR;
			}
		}
		if (fac1->mantissa[0] < 10) {
			lshift_7(fac1, dwords);
			fac1->exponent -= 7;
		}
		else if (fac1->mantissa[0] < 100) {
			lshift_6(fac1, dwords);
			fac1->exponent -= 6;
		}
		else if (fac1->mantissa[0] < 1000) {
			lshift_5(fac1, dwords);
			fac1->exponent -= 5;
		}
		else if (fac1->mantissa[0] < 10000) {
			lshift_4(fac1, dwords);
			fac1->exponent -= 4;
		}
		else if (fac1->mantissa[0] < 100000) {
			lshift_3(fac1, dwords);
			fac1->exponent -= 3;
		}
		else if (fac1->mantissa[0] < 1000000) {
			lshift_2(fac1, dwords);
			fac1->exponent -= 2;
		}
		else if (fac1->mantissa[0] < 10000000) {
			lshift_1(fac1, dwords);
			fac1->exponent -= 1;
		}
	}
	//check for overflow/underflow
	if (fac1->exponent < 0) {
		return EXPO_ERR;
	}
	return 0;
}

void fpadd_aux(DecFloat_struct* fac1, DecFloat_struct* fac2, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;
	int32_t v, c, i;
	c = 0;
	for (i = dwords; i > 0; i--) {
		v = fac2->mantissa[i] + fac1->mantissa[i] + c;
		if (v > 99999999) {
			v = v - 100000000;
			c = 1;
		}
		else {
			c = 0;
		}
		fac1->mantissa[i] = v;
	}
	v = fac1->mantissa[0] + fac2->mantissa[0] + c;
	fac1->mantissa[0] = v;
	i = norm_fac1(fac1, dwords);
}

void fpsub_aux(DecFloat_struct* fac1, DecFloat_struct* fac2, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;
	int32_t v, c, i;
	c = 0;
	for (i = dwords; i > 0; i--) {
		v = fac1->mantissa[i] - fac2->mantissa[i] - c;
		if (v < 0) {
			v = v + 100000000;
			c = 1;
		}
		else {
			c = 0;
		}
		fac1->mantissa[i] = v;
	}
	v = fac1->mantissa[0] - fac2->mantissa[0] - c;
	fac1->mantissa[0] = v;
	i = norm_fac1(fac1, dwords);
}

void fpadd(DecFloat_struct* result, DecFloat_struct* x, DecFloat_struct* y, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;

	DecFloat_struct fac1, fac2;
	int32_t i, t, c, xsign, ysign;

	DecFloat_init(&fac1, NUM_DIGITS);
	DecFloat_init(&fac2, NUM_DIGITS);
	xsign = x->sign; x->sign = 0;
	ysign = y->sign; y->sign = 0;
	c = fpcmp(x, y, dwords);

	x->sign = xsign;
	y->sign = ysign;
	if (c < 0) {
		copydf(&fac1, y, dwords);
		copydf(&fac2, x, dwords);
	}
	else {
		copydf(&fac1, x, dwords);
		copydf(&fac2, y, dwords);
	}
	t = fac1.exponent - fac2.exponent;

	t = ((fac1.exponent & 0x7fffffff) - BIAS - 1) - ((fac2.exponent & 0x7fffffff) - BIAS - 1);

	if (t < (NUM_DIGITS + 8 * (1+EXTRAWORDS))) {
		//the difference between the two
		//exponents indicate how many times
		//we have to multiply the mantissa
		//of fac2 by 10 (i.e., shift it right 1 place).
		//if we have to shift more times than
		//we have dwords, the result is already in fac1.
		t = fac1.exponent - fac2.exponent;
		if ((t > 0) && (t < (NUM_DIGITS + 8 * (1+EXTRAWORDS)))) { //shift
			i = t / 8;
			while (i > 0) {
				rshift_8(&fac2, dwords);
				t -= 8;
				i -= 1;
			}
			if (t == 7) rshift_7(&fac2, dwords);
			else if (t == 6) rshift_6(&fac2, dwords);
			else if (t == 5) rshift_5(&fac2, dwords);
			else if (t == 4) rshift_4(&fac2, dwords);
			else if (t == 3) rshift_3(&fac2, dwords);
			else if (t == 2) rshift_2(&fac2, dwords);
			else if (t == 1) rshift_1(&fac2, dwords);
		}
		//see if the signs of the two numbers
		//are the same.  if so, add; if not, subtract.
		if (fac1.sign == fac2.sign) { //add
			fpadd_aux(&fac1, &fac2, dwords);
		}
		else {
			fpsub_aux(&fac1, &fac2, dwords);
		}
	}
	copydf(result, &fac1, dwords);
	DecFloat_clear(&fac2);
	DecFloat_clear(&fac1);
}

void fpsub(DecFloat_struct* result, DecFloat_struct* x, DecFloat_struct* y, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;
	DecFloat_struct fac1, fac2;
	DecFloat_init(&fac1, NUM_DIGITS);
	DecFloat_init(&fac2, NUM_DIGITS);
	copydf(&fac1, x, dwords);
	copydf(&fac2, y, dwords);
	fac2.sign = (fac2.sign ^ (-1));
	fpadd(&fac1, &fac1, &fac2, dwords);
	copydf(result, &fac1, dwords);
	DecFloat_clear(&fac2);
	DecFloat_clear(&fac1);
}

void fpmul(DecFloat_struct* result, DecFloat_struct* x, DecFloat_struct* y, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;
	DecFloat_struct fac1, fac2;
	int32_t i, j, ex = 0, den, num; //, er;
	int64_t digit, carry, prod;
	uint32_t* fac3;
	fac3 = (uint32_t*)calloc((2 * (uint64_t)dwords + 3), sizeof(uint32_t));
	//uint32_t fac3[dwords*2+2];

	DecFloat_init(&fac1, NUM_DIGITS);
	DecFloat_init(&fac2, NUM_DIGITS);
	copydf(&fac1, x, dwords);
	copydf(&fac2, y, dwords);

	//check exponents.  if either is zero,
	//the result is zero
	if ((fac1.exponent == 0) || (fac2.exponent == 0)) { //result is zero...clear fac1.
		fac1.sign = 0;
		fac1.exponent = 0;
		for (i = 0; i <= dwords; i++) {
			fac1.mantissa[i] = 0;
		}
		//norm_fac1(fac1)
		copydf(result, &fac1, dwords);
		DecFloat_clear(&fac2);
		DecFloat_clear(&fac1);
		free(fac3); fac3 = 0;
		return;
	}
	else {
		if (ex < 0) {
			//er=EXPO_ERR;
			copydf(result, &fac1, dwords);
			DecFloat_clear(&fac2);
			DecFloat_clear(&fac1);
			free(fac3); fac3 = 0;
			return; //exit function
		}

		//clear fac3 mantissa
		for (i = 0; i <= (dwords * 2 + 1); i++) {
			fac3[i] = 0;
		}

		den = dwords;
		while (fac2.mantissa[den] == 0) {
			den -= 1;
		}
		num = dwords;
		while (fac1.mantissa[num] == 0) {
			num -= 1;
		}

		if (num < den) {
			swapdf(&fac1, &fac2);
			copydf(&fac1, y, dwords);
			//copydf(&fac2, x, dwords);		
			//swap den, num
			i = den; den = num;
			num = i;
		}

		for (j = den; j >= 0; j--) {
			carry = 0;
			digit = fac2.mantissa[j];
			for (i = num; i >= 0; i--) {
				prod = fac3[i + j + 1] + digit * fac1.mantissa[i] + carry;
				carry = prod / 100000000ll;
				fac3[i + j + 1] = (prod % 100000000ll);
			}

			fac3[j] = (uint32_t)carry;
		}

		for (i = 0; i <= dwords; i++) {
			fac1.mantissa[i] = fac3[i];
		}
	}
	//now determine exponent of result.
	//as you do...watch for overflow.
	ex = fac2.exponent - BIAS + fac1.exponent;
	fac1.exponent = ex;
	//determine the sign of the product
	fac1.sign = fac1.sign ^ fac2.sign;
	// if you wanted the result of the multiplication to be rounded up then you would un-comment the following
	/*
	if (fac3[dwords+1]>=50000000) {
		carry=1;
		for (i=dwords; i>=0; i--) {
			j=fac1.mantissa[i]+carry;
			if (j>99999999) {
				j=j-100000000;
				carry=1;
			}
			else {
				carry=0;
			}
			fac1.mantissa[i]=j;
			if (carry==0) break;
		}
	}
	*/
	i = norm_fac1(&fac1, dwords);
	copydf(result, &fac1, dwords);
	DecFloat_clear(&fac2);
	DecFloat_clear(&fac1);
	free(fac3); fac3 = 0;
}

void fpmul_si(DecFloat_struct* result, DecFloat_struct* x, int64_t y, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;
	DecFloat_struct fac1, fac2;
	int32_t count, i;
	int64_t carry, digit, prod, value;

	DecFloat_init(&fac1, NUM_DIGITS);
	DecFloat_init(&fac2, NUM_DIGITS);
	copydf(&fac1, x, dwords);

	if (y < 0) {
		digit = -y;
	}
	else {
		digit = y;
	}
	if (digit > 99999999) {
		si2fp(&fac2, y, dwords);
		fpmul(result, &fac1, &fac2, dwords);
		DecFloat_clear(&fac2);
		DecFloat_clear(&fac1);
		return;
	}
	//check exponents.  if either is zero,
	//the result is zero
	if ((fac1.exponent == 0) || (y == 0)) { //result is zero...clear fac1.
		fac1.sign = 0;
		fac1.exponent = 0;
		for (count = 0; count <= dwords; count++) {
			result->mantissa[count] = 0;
		}
		DecFloat_clear(&fac2);
		DecFloat_clear(&fac1);
		return;
	}
	else {
		if (digit == 1) {
			if (y < 0) {
				fac1.sign = fac1.sign ^ (-1);
			}
			copydf(result, &fac1, dwords);
			DecFloat_clear(&fac2);
			DecFloat_clear(&fac1);
			return;
		}

		carry = 0;

		for (i = dwords; i >= 0; i--) {
			prod = digit * fac1.mantissa[i] + carry;
			value = (prod % 100000000ll);
			fac1.mantissa[i] = (uint32_t)value;
			carry = prod / 100000000ll;
		}

		if (carry < 10) {
			rshift_1(&fac1, dwords);
			fac1.exponent += 1;
			fac1.mantissa[0] += (uint32_t)carry * 10000000;
		}
		else if (carry < 100) {
			rshift_2(&fac1, dwords);
			fac1.exponent += 2;
			fac1.mantissa[0] += (uint32_t)carry * 1000000;
		}
		else if (carry < 1000) {
			rshift_3(&fac1, dwords);
			fac1.exponent += 3;
			fac1.mantissa[0] += (uint32_t)carry * 100000;
		}
		else if (carry < 10000) {
			rshift_4(&fac1, dwords);
			fac1.exponent += 4;
			fac1.mantissa[0] += (uint32_t)carry * 10000;
		}
		else if (carry < 100000) {
			rshift_5(&fac1, dwords);
			fac1.exponent += 5;
			fac1.mantissa[0] += (uint32_t)carry * 1000;
		}
		else if (carry < 1000000) {
			rshift_6(&fac1, dwords);
			fac1.exponent += 6;
			fac1.mantissa[0] += (uint32_t)carry * 100;
		}
		else if (carry < 10000000) {
			rshift_7(&fac1, dwords);
			fac1.exponent += 7;
			fac1.mantissa[0] += (uint32_t)carry * 10;
		}
		else if (carry < 100000000) {
			rshift_8(&fac1, dwords);
			fac1.exponent += 8;
			fac1.mantissa[0] += (uint32_t)carry;
		}

	}

	i = norm_fac1(&fac1, dwords);

	if (y < 0) {
		fac1.sign = fac1.sign ^ (-1);
	}
	copydf(result, &fac1, dwords);
	DecFloat_clear(&fac2);
	DecFloat_clear(&fac1);
}

void LSHIFT_da(double* mantissa, int32_t n, int32_t k) {
	uint32_t v1, v2, c1, c2;
	int32_t i;
	c1 = (uint32_t)p10_constants[k];
	c2 = (uint32_t)p10_constants[4 - k];
	for (i = 2; i <= (n - 1); i++) {
		v1 = ((uint32_t)mantissa[i]) % c2;
		v2 = (uint32_t)mantissa[i + 1] / c2;
		mantissa[i] = v1 * c1 + v2;
		mantissa[i + 1] = (uint32_t)(mantissa[i + 1]) % c2;
	}
	mantissa[n] = c1 * ((uint32_t)mantissa[n] % c2);
}

#define lngmin(a, b)(((a) < (b)) ? (a) : (b))

double realw(double* w, int32_t j, int32_t ubw) {
	double wx;
	wx = ((w[j - 1] * 10000 + w[j]) * 10000 + w[j + 1]) * 10000;
	if (ubw >= (j + 2)) {
		wx = wx + w[j + 2];
	}
	return wx;
}

void subtract(double* w, int32_t q, double* d, int32_t ka, int32_t kb) {
	int32_t j;
	for (j = ka; j <= kb; j++) {
		w[j] = w[j] - q * d[j - ka + 2];
	}
}
//int result = (var1 > var2) ? var1 : var2;

void normalize(double* w, int32_t ka, int32_t q) {
	w[ka] = w[ka] + w[ka - 1] * 10000;
	w[ka - 1] = q;
}

void finalnorm(double* w, int32_t kb) {
	const int32_t b = 10000;
	int32_t carry, j;
	for (j = kb; j >= 3; j--) {
		carry = (w[j] < 0) ? (int32_t)((-floor(w[j]) - 1) / b) + 1 : (int32_t)(w[j] >= b) ? (int32_t)(-floor(w[j] / b)) : 0;
		w[j] = w[j] + carry * b;
		w[j - 1] = w[j - 1] - carry;
	}
}

// a multiple-precision division algorithm http://dmsmith.lmu.build/mcomp1996.pdf

void fpdiv(DecFloat_struct* result, DecFloat_struct* x, DecFloat_struct* y, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;
	DecFloat_struct fac1, fac2;
	int32_t i, is_power_of_ten;

	DecFloat_init(&fac1, NUM_DIGITS);
	DecFloat_init(&fac2, NUM_DIGITS);
	copydf(&fac1, x, dwords);
	copydf(&fac2, y, dwords);

	if (fac2.exponent == 0) { // if fac2 = 0, return
		// a divide-by-zero error and
		// bail out.
		for (i = 0; i <= dwords; i++) {
			fac1.mantissa[i] = 99999999;
		}
		fac1.exponent = 99999 + BIAS + 1;
		//er=DIVZ_ERR;
		copydf(result, &fac1, dwords);
		DecFloat_clear(&fac2);
		DecFloat_clear(&fac1);
		return;
	}
	else if (fac1.exponent == 0) { //fact1=0, just return
		//er=0;
		copydf(result, &fac1, dwords);
		DecFloat_clear(&fac2);
		DecFloat_clear(&fac1);
		return;
	}
	else {
		//check to see if fac2 is a power of ten
		is_power_of_ten = 0;
		if (fac2.mantissa[0] == 10000000) {
			is_power_of_ten = 1;
			for (i = 1; i <= dwords; i++) {
				if (fac2.mantissa[i] != 0) {
					is_power_of_ten = 0;
					break;
				}
			}
		}
		//if fac2 is a power of ten then all we need to do is to adjust the sign and exponent and we are finished
		if (is_power_of_ten == 1) {
			fac1.sign = fac1.sign ^ fac2.sign;
			fac1.exponent = fac1.exponent - fac2.exponent + BIAS + 1;
			copydf(result, &fac1, dwords);
			DecFloat_clear(&fac2);
			DecFloat_clear(&fac1);
			return;
		}
		const int32_t  ubn = 2 * dwords + 4;
		const int32_t ubw = ubn + 5;
		double* resul;
		double* n;
		double* d;
		double* w;
		w = (double*)calloc((ubw + 1), sizeof(double));
		resul = (double*)calloc((ubn + 5), sizeof(double));
		n = (double*)calloc((ubn + 5), sizeof(double));
		d = (double*)calloc((ubn + 5), sizeof(double));
		const int32_t b = 10000;
		int32_t j, last, laststep, q, t;
		int32_t stp; //, carry;
		double xd, xn, rund;
		//double w[ubw];

		for (j = 0; j <= dwords; j++) {
			n[2 * j + 2] = fac1.mantissa[j] / 10000;
			n[2 * j + 3] = fac1.mantissa[j] % 10000;
			d[2 * j + 2] = fac2.mantissa[j] / 10000;
			d[2 * j + 3] = fac2.mantissa[j] % 10000;
		}
		n[1] = (fac1.exponent & 0x7fffffff) - BIAS - 1;
		d[1] = (fac2.exponent & 0x7fffffff) - BIAS - 1;
		for (j = ubn; j <= ubw; j++) {
			w[j] = 0;
		}
		t = ubn - 1;
		w[1] = n[1] - d[1] + 1;
		w[2] = 0;
		for (j = 2; j <= ubn; j++) {
			w[j + 1] = n[j];
		}
		xd = (d[2] * b + d[3]) * b + d[4] + d[5] / b;
		laststep = t + 2;
		for (stp = 1; stp <= laststep; stp++) {
			xn = realw(w, (stp + 2), ubw);
			q = (int32_t)floor(xn / xd);
			last = lngmin(stp + t + 1, ubw);
			subtract(w, q, d, (stp + 2), last);
			normalize(w, (stp + 2), q);
		}
		finalnorm(w, (laststep + 1));
		laststep = (w[2] == 0) ? laststep : laststep - 1;
		rund = w[laststep + 1] / b;
		w[laststep] = w[laststep] + ((rund >= 0.5) ? 1 : 0);
		if (w[2] == 0) {
			for (j = 1; j <= (t + 1); j++) {
				resul[j] = w[j + 1];
			}
		}
		else {
			for (j = 1; j <= (t + 1); j++) {
				resul[j] = w[j];
			}
		}
// left-shift out leading 0's if any to preserve accuracy
#if 1
		j = 0;
		if (resul[2] < 10) {
			j = 3;
		}
		else if (resul[2] < 100) {
			j = 2;
		}
		else if (resul[2] < 1000) {
			j = 1;
		}

		if (j > 0) {
			LSHIFT_da(resul, 2 * dwords + 4, j);
		}
#endif
		resul[1] = (w[2] == 0) ? w[1] - 1 : w[1];

		for (j = 0; j <= dwords; j++) {
			fac1.mantissa[j] = (uint32_t)(resul[2 * j + 2] * 10000 + resul[2 * j + 3]);
		}
		j = norm_fac1(&fac1, dwords);
		fac1.exponent = (uint32_t)(resul[1] + BIAS);
		free(resul); resul = 0;
		free(w); w = 0;
		free(n); n = 0;
		free(d); d = 0;
	}
	fac1.sign = fac1.sign ^ fac2.sign;
	copydf(result, &fac1, dwords);
	DecFloat_clear(&fac2);
	DecFloat_clear(&fac1);
}

// a version of square root that doesn't use log and exp
// sqrt(num)
void fpsqr(DecFloat_struct* result, DecFloat_struct* num, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;
	DecFloat_struct r, r2, tmp, n, half;
	int32_t ex, k, l, prec;
	//dim as string ts, v
	double x, y;

	DecFloat_init(&r, NUM_DIGITS);
	DecFloat_init(&r2, NUM_DIGITS);
	DecFloat_init(&tmp, NUM_DIGITS);
	DecFloat_init(&n, NUM_DIGITS);
	DecFloat_init(&half, NUM_DIGITS);
	l = (int32_t)(log((dwords + 1) * 8 * 0.0625) * 1.5) + 1;
	//l=estimated number of iterations needed
	//first estimate is accurate to about 16 digits
	//l is approximatly = to log2((NUM_DIGITS+9)/16)
	//NUM_DIGITS+9 because decfloat has an extra 9 guard digits
	copydf(&n, num, dwords);

	if ((fpcmp(&n, &tmp, dwords) == 0) || (fpcmp(&n, &tmp, dwords) < 0)) {
		si2fp(result, 0, dwords);
		DecFloat_clear(&half);
		DecFloat_clear(&n);
		DecFloat_clear(&tmp);
		DecFloat_clear(&r2);
		DecFloat_clear(&r);
		return;
	}
	si2fp(result, 1, dwords);
	if (fpcmp(&n, result, dwords) == 0) {
		DecFloat_clear(&half);
		DecFloat_clear(&n);
		DecFloat_clear(&tmp);
		DecFloat_clear(&r2);
		DecFloat_clear(&r);
		return;
	}

	//=====================================================================
	//hack to bypass the limitation of double exponent range
	//in case the number is larger than what a double can handle
	//for example, if the number is 2e500
	//we separate the exponent and mantissa in this case 2
	//if the exponent is odd then multiply the mantissa by 10
	//take the square root and assign it to decfloat
	//divide the exponent in half for square root
	//in this case 1.414213562373095e250
	if (n.exponent > 0) {
		ex = (n.exponent & 0x7fffffff) - BIAS - 1;
	}
	else {
		ex = 0;
	}

	x = n.mantissa[0] + (n.mantissa[1] / 100000000.0);
	x = x / 10000000;

	if (x == 0) {
		si2fp(result, 0, dwords);
		DecFloat_clear(&half);
		DecFloat_clear(&n);
		DecFloat_clear(&tmp);
		DecFloat_clear(&r2);
		DecFloat_clear(&r);
		return;
	}
	if ((x == 1) && (ex == 0)) {
		si2fp(result, 1, dwords);
		DecFloat_clear(&half);
		DecFloat_clear(&n);
		DecFloat_clear(&tmp);
		DecFloat_clear(&r2);
		DecFloat_clear(&r);
		return;
	}
	if ((abs(ex) % 2) != 0) {
		x = x * 10;
		ex -= 1;
	}
	x = sqrt(x); //approximation
	y = x * 10000000;
	r.mantissa[0] = (uint32_t)(floor(y));
	r.mantissa[1] = (uint32_t)((y - r.mantissa[0]) * 100000000);
	r.exponent = ex / 2 + BIAS + 1;
	//if len(v)>1 and k=0 then r.exponent+=1
	for (k = 1; k <= dwords; k++) {
		half.mantissa[k] = 0;
	}
	half.mantissa[0] = 50000000;
	half.exponent = BIAS;
	half.sign = 0;

	//=====================================================================
	//newton-raphson method

	prec = 3;
	for (k = 1; k <= (l + 1); k++) {
		prec = 2 * prec - 1;
		fpdiv(&tmp, &n, &r, prec);
		fpadd(&r2, &r, &tmp, prec);
		fpmul(&r, &r2, &half, prec);
	}
	copydf(result, &r, dwords);
	DecFloat_clear(&half);
	DecFloat_clear(&n);
	DecFloat_clear(&tmp);
	DecFloat_clear(&r2);
	DecFloat_clear(&r);
}

void fpsqr2(DecFloat_struct* result, DecFloat_struct* num, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	DecFloat_struct ry, tmp, tmp2;
	double t, t1, t2;
	int32_t i, ex, l, prec, p = 2;

	DecFloat_init(&ry, NUM_DIGITS);
	DecFloat_init(&tmp, NUM_DIGITS);
	DecFloat_init(&tmp2, NUM_DIGITS);

	l = (int32_t)(log((dwords * 8) * 0.0625) * 1.5 + 1);
	ex = (num->exponent & 0x7fffffff) - BIAS - 1;
	t = num->mantissa[0] + num->mantissa[1] / 100000000;
	t = t / 10000000;
	t1 = log(t) / p;
	t2 = 2.302585092994046 * ex / p;
	t1 = exp(t1);
	dbl2fp(&ry, t2, dwords); //ry=str2fp( str( t2 ) ) //ry=dbl2fp(t2)
	fpexp_aux(&ry, &ry, 2);
	dbl2fp(&tmp, t1, dwords); //tmp=str2fp( str( t1 ) ) //dbl2fp(t1)
	fpmul(&ry, &ry, &tmp, 2);
	prec = 3;

	fpdiv(&tmp, num, &ry, prec);
	fpadd(&tmp2, &ry, &tmp, prec);
	fpdiv_si(&ry, &tmp2, p, prec);

	for (i = 1; i <= l; i++) {
		prec = 2 * prec - 1;
		fpdiv(&tmp, num, &ry, prec);
		fpadd(&tmp2, &ry, &tmp, prec);
		fpdiv_si(&ry, &tmp2, p, prec);
	}
	copydf(result, &ry, dwords);
	DecFloat_clear(&ry);
	DecFloat_clear(&tmp);
	DecFloat_clear(&tmp2);
}

void fpdiv_si(DecFloat_struct* result, DecFloat_struct* num, int32_t den, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	DecFloat_struct fac1;
	uint64_t carry, remder = 0;
	int64_t divisor;
	int64_t quotient;
	int32_t i;

	if (den < 0) {
		divisor = -den;
	}
	else {
		divisor = den;
	}
	DecFloat_init(&fac1, NUM_DIGITS);
	copydf(&fac1, num, dwords + 1);
	if (divisor == 0) {
		//printf("%s\n", "error: divisor = 0");
		for (i = 0; i <= dwords; i++) {
			result->mantissa[i] = 99999999;
		}
		result->exponent = 99999999;
		result->sign = num->sign;
		DecFloat_clear(&fac1);
		return;
	}
	if (divisor > 99999999) {
		DecFloat_struct fac2;
		DecFloat_init(&fac2, NUM_DIGITS);
		si2fp(&fac2, den, dwords);
		fpdiv(result, &fac1, &fac2, dwords);
		DecFloat_clear(&fac2);
		DecFloat_clear(&fac1);
		return;
	}

	for (i = 0; i <= (dwords); i++) {
		quotient = fac1.mantissa[i] + remder * 100000000ll;
		remder = quotient % divisor;
		fac1.mantissa[i] = (uint32_t)floor((double)(quotient / divisor));
	}
	quotient = remder * 100000000ll;
	quotient = (int64_t)floor((double)(quotient / divisor));
	carry = fac1.mantissa[0];

	if (carry == 0) {
		lshift_8(&fac1, dwords);
		fac1.exponent -= 8;
		fac1.mantissa[dwords] += (uint32_t)quotient;
	}
	else if (carry < 10) {
		lshift_7(&fac1, dwords);
		fac1.exponent -= 7;
		fac1.mantissa[dwords] += (uint32_t)floor((double)(quotient / 10));
	}
	else if (carry < 100) {
		lshift_6(&fac1, dwords);
		fac1.exponent -= 6;
		fac1.mantissa[dwords] += (uint32_t)floor((double)(quotient / 100));
	}
	else if (carry < 1000) {
		lshift_5(&fac1, dwords);
		fac1.exponent -= 5;
		fac1.mantissa[dwords] += (uint32_t)floor((double)(quotient / 1000));
	}
	else if (carry < 10000) {
		lshift_4(&fac1, dwords);
		fac1.exponent -= 4;
		fac1.mantissa[dwords] += (uint32_t)floor((double)(quotient / 10000));
	}
	else if (carry < 100000) {
		lshift_3(&fac1, dwords);
		fac1.exponent -= 3;
		fac1.mantissa[dwords] += (uint32_t)floor((double)(quotient / 100000));
	}
	else if (carry < 1000000) {
		lshift_2(&fac1, dwords);
		fac1.exponent -= 2;
		fac1.mantissa[dwords] += (uint32_t)floor((double)(quotient / 1000000));
	}
	else if (carry < 10000000) {
		lshift_1(&fac1, dwords);
		fac1.exponent -= 1;
		fac1.mantissa[dwords] += (uint32_t)floor((double)(quotient / 10000000));
	}

	i = norm_fac1(&fac1, dwords);
	if (den < 0) {
		fac1.sign = fac1.sign ^ (-1);
	}
	//i=norm_fac1(&fac1, dwords);
	copydf(result, &fac1, dwords);
	DecFloat_clear(&fac1);
}


void fpipow(DecFloat_struct* result, DecFloat_struct* x, int64_t e, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;
	//take x to an long power
	DecFloat_struct one, y, r;
	int64_t n; //, c = 0;

	DecFloat_init(&one, NUM_DIGITS);
	DecFloat_init(&y, NUM_DIGITS);
	DecFloat_init(&r, NUM_DIGITS);

	copydf(&y, x, dwords);
	if (e < 0) {
		n = -e;
	}
	else {
		n = e;
	}
	r.sign = 0;
	r.exponent = (BIAS + 1);
	r.mantissa[0] = 10000000;
	copydf(&one, &r, dwords);
	while (n > 0) {
		while ((n & 1) == 0) {
			n >>= 1;
			fpmul(&y, &y, &y, dwords);
			//c += 1;
		}
		n -= 1;
		fpmul(&r, &y, &r, dwords);
		//c += 1;
	}
	if (e < 0) {
		fpdiv(&r, &one, &r, dwords);
	}
	copydf(result, &r, dwords);
	DecFloat_clear(&r);
	DecFloat_clear(&y);
	DecFloat_clear(&one);
}

void dbl2fp(DecFloat_struct* result, double x, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;

	int32_t n;
	double y, z;
	DecFloat_struct fp, w;
	DecFloat_init(&fp, NUM_DIGITS);
	DecFloat_init(&w, NUM_DIGITS);
	y = frexp(fabs(x), &n);
	z = y * 100000000;
	result->mantissa[0] = (uint32_t)trunc(z);
	z = z - result->mantissa[0];
	z = z * 100000000;
	result->mantissa[1] = (uint32_t)trunc(z);
	result->exponent = BIAS;
	result->sign = 0;
	norm_fac1(result, 2);
	if (x < 0) result->sign = -1;
	si2fp(&fp, 2, dwords);
	fpipow(&w, &fp, n, dwords);
	fpmul(result, result, &w, dwords);
	DecFloat_clear(&w);
	DecFloat_clear(&fp);
}

void ldbl2fp(DecFloat_struct* result, long double x, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;

	int32_t n;
	long double y, z;
	DecFloat_struct fp, w;
	DecFloat_init(&fp, NUM_DIGITS);
	DecFloat_init(&w, NUM_DIGITS);
	y = frexpl(fabsl(x), &n);
	z = y * 100000000;
	result->mantissa[0] = (uint32_t)truncl(z);
	z = z - result->mantissa[0];
	z = z * 100000000;
	result->mantissa[1] = (uint32_t)truncl(z);
	z = z - result->mantissa[1];
	z = z * 100000000;
	result->mantissa[2] = (uint32_t)truncl(z);
	result->exponent = BIAS;
	result->sign = 0;
	norm_fac1(result, 2);
	if (x < 0) result->sign = -1;
	si2fp(&fp, 2, dwords);
	fpipow(&w, &fp, n, dwords);
	fpmul(result, result, &w, dwords);
	DecFloat_clear(&w);
	DecFloat_clear(&fp);
}

void fpexp_aux(DecFloat_struct* result, DecFloat_struct* x, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;

	DecFloat_struct  fac, x2, temp, accum, p, term;
	int32_t     i, c;

	DecFloat_init(&fac, NUM_DIGITS);
	DecFloat_init(&x2, NUM_DIGITS);
	DecFloat_init(&temp, NUM_DIGITS);
	DecFloat_init(&accum, NUM_DIGITS);
	DecFloat_init(&p, NUM_DIGITS);
	DecFloat_init(&term, NUM_DIGITS);

	si2fp(&temp, 0, dwords);
	si2fp(&fac, 1, dwords);

	if (fpcmp(x, &temp, dwords) == 0) {
		copydf(result, &fac, dwords);
		return;
	}
	c = 1;
	fpdiv_si(&x2, x, 67108864, dwords);

	copydf(&p, &x2, dwords);
	fpadd(&accum, &fac, &x2, dwords);
	for (;;) {
		c += 1;
		copydf(&temp, &accum, dwords);
		fpdiv_si(&fac, &fac, c, dwords);
		fpmul(&p, &p, &x2, dwords);
		fpmul(&term, &p, &fac, dwords);
		fpadd(&accum, &temp, &term, dwords);
		if (fpcmp(&accum, &temp, dwords) == 0) {
			break;
		}
	}

	for (i = 1; i <= 26; i++) {
		fpmul(&accum, &accum, &accum, dwords);
	}
	copydf(result, &accum, dwords);

	DecFloat_clear(&term);
	DecFloat_clear(&p);
	DecFloat_clear(&accum);
	DecFloat_clear(&temp);
	DecFloat_clear(&x2);
	DecFloat_clear(&fac);
}

void agm(DecFloat_struct* result, DecFloat_struct* a_in, DecFloat_struct* b_in, int32_t digits) {
	if (digits < 16) {
		digits = 16;
	}
	if (digits > (NUMBER_OF_DIGITS + 8)) {
		digits = NUMBER_OF_DIGITS + 8;
	}

	int32_t     dwords;
	DecFloat_struct  a;
	DecFloat_struct  b;
	DecFloat_struct  c;
	DecFloat_struct  d;
	DecFloat_struct  eps;
	DecFloat_struct  diff;

	DecFloat_init(&a, NUM_DIGITS);
	DecFloat_init(&b, NUM_DIGITS);
	DecFloat_init(&c, NUM_DIGITS);
	DecFloat_init(&d, NUM_DIGITS);
	DecFloat_init(&eps, NUM_DIGITS);
	DecFloat_init(&diff, NUM_DIGITS);
	dwords = (int32_t)(digits / 8);

	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;

	copydf(&a, a_in, dwords);
	copydf(&b, b_in, dwords);
	eps.exponent = (uint32_t)(-(digits)+BIAS + 1);
	eps.mantissa[0] = 10000000ul;

	for (;;) {
		fpadd(&c, &a, &b, dwords);
		fpdiv_si(&c, &c, 2, dwords);
		fpmul(&d, &a, &b, dwords);
		fpsqr(&d, &d, dwords);
		copydf(&a, &c, dwords);
		copydf(&b, &d, dwords);
		fpsub(&diff, &a, &b, dwords);
		diff.sign = 0;
		if (fpcmp(&diff, &eps, dwords) <= 0) {
			break;
		}
	}

	copydf(result, &a, dwords);
	DecFloat_clear(&diff);
	DecFloat_clear(&eps);
	DecFloat_clear(&d);
	DecFloat_clear(&c);
	DecFloat_clear(&b);
	DecFloat_clear(&a);
}

void agm1(DecFloat_struct* in1, DecFloat_struct* in2, DecFloat_struct* out1, DecFloat_struct* out2) {
	fpadd(out1, in1, in2, NUM_DWORDS + EXTRAWORDS);
	fpdiv_si(out1, out1, 2, NUM_DWORDS + EXTRAWORDS);
	fpmul(out2, in1, in2, NUM_DWORDS + EXTRAWORDS);
	fpsqr(out2, out2, NUM_DWORDS + EXTRAWORDS);
}

void piagm(DecFloat_struct* pi) {
	DecFloat_struct x0, y0, resA, resB, Z, var;
	DecFloat_init(&x0, NUM_DIGITS);
	DecFloat_init(&y0, NUM_DIGITS);
	DecFloat_init(&resA, NUM_DIGITS);
	DecFloat_init(&resB, NUM_DIGITS);
	DecFloat_init(&Z, NUM_DIGITS);
	DecFloat_init(&var, NUM_DIGITS);
	si2fp(&x0, 1, NUM_DWORDS + EXTRAWORDS);
	dbl2fp(&y0, 0.5, NUM_DWORDS + EXTRAWORDS);
	fpsqr(&y0, &y0, NUM_DWORDS + EXTRAWORDS);
	dbl2fp(&Z, 0.25, NUM_DWORDS + EXTRAWORDS);

	int n = 1;
	int i;
	for (i = 0; i < 8; i++) {
		agm1(&x0, &y0, &resA, &resB);
		fpsub(&var, &resA, &x0, NUM_DWORDS + EXTRAWORDS);
		fpmul(&var, &var, &var, NUM_DWORDS + EXTRAWORDS);
		fpmul_si(&var, &var, n, NUM_DWORDS + EXTRAWORDS);
		fpsub(&Z, &Z, &var, NUM_DWORDS + EXTRAWORDS);
		n += n;
		agm1(&resA, &resB, &x0, &y0);
		fpsub(&var, &x0, &resA, NUM_DWORDS + EXTRAWORDS);
		fpmul(&var, &var, &var, NUM_DWORDS + EXTRAWORDS);
		fpmul_si(&var, &var, n, NUM_DWORDS + EXTRAWORDS);
		fpsub(&Z, &Z, &var, NUM_DWORDS + EXTRAWORDS);
		n += n;
	}
	fpmul(&x0, &x0, &x0, NUM_DWORDS + EXTRAWORDS);
	fpdiv(pi, &x0, &Z, NUM_DWORDS + EXTRAWORDS);
	DecFloat_clear(&var);
	DecFloat_clear(&Z);
	DecFloat_clear(&resB);
	DecFloat_clear(&resA);
	DecFloat_clear(&y0);
	DecFloat_clear(&x0);
}

void BSPi_Ramanujan(uint64_t a, uint64_t b, DecFloat_struct* p, DecFloat_struct* q, DecFloat_struct* r, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	
	DecFloat_struct  pp;
	DecFloat_struct  qq;
	DecFloat_struct  rr;
	DecFloat_struct  tmp;

	DecFloat_init(&pp, NUM_DIGITS);
	DecFloat_init(&qq, NUM_DIGITS);
	DecFloat_init(&rr, NUM_DIGITS);
	DecFloat_init(&tmp, NUM_DIGITS);
	uint64_t  md;
	if ((b - a) == 1) {
		if (b <= 832256) {
			si2fp(r, ((2 * b) - 1), dwords);
			fpmul_si(r, r, ((4 * b) - 3), dwords);
			fpmul_si(r, r, ((4 * b) - 1), dwords);
		}
		else {
			si2fp(r, ((4 * b) - 3), dwords);
			fpmul_si(r, r, ((4 * b) - 1), dwords);
			fpmul_si(r, r, ((2 * b) - 1), dwords);
		}
		si2fp(p, 1103ll + (26390ll * b), dwords);
		fpmul(p, p, r, dwords);
		si2fp(q, b, dwords);
		fpmul(&tmp, q, q, dwords);
		fpmul(q, q, &tmp, dwords);
		si2fp(&tmp, 3073907232ll, dwords);
		fpmul(q, q, &tmp, dwords);
		return;
	}
	md = (a + b) / 2;
	BSPi_Ramanujan(a, md, p, q, r, dwords);
	BSPi_Ramanujan(md, b, &pp, &qq, &rr, dwords);
	fpmul(&tmp, &pp, r, dwords);
	fpmul(p, p, &qq, dwords);
	fpadd(p, p, &tmp, dwords);
	fpmul(q, q, &qq, dwords);
	fpmul(r, r, &rr, dwords);

	DecFloat_clear(&tmp);
	DecFloat_clear(&rr);
	DecFloat_clear(&qq);
	DecFloat_clear(&pp);
}

void Pi_RamanujanBS(DecFloat_struct* pi, int32_t digits) {
	uint64_t  k;
	int32_t dwords = (int32_t)(digits * 0.125) + 1;
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	DecFloat_struct  p;
	DecFloat_struct  q;
	DecFloat_struct  r;
	DecFloat_struct  tmp;
	DecFloat_struct  tmp2;

	DecFloat_init(&p, NUM_DIGITS);
	DecFloat_init(&q, NUM_DIGITS);
	DecFloat_init(&r, NUM_DIGITS);
	DecFloat_init(&tmp, NUM_DIGITS);
	DecFloat_init(&tmp2, NUM_DIGITS);


	k = (uint64_t)ceil(digits * log(10) / log(96059301ll));
	BSPi_Ramanujan(0, k, &p, &q, &r, dwords);
	fpmul_si(pi, &q, 9801ll, dwords);
	fpmul_si(&tmp, &q, 1103ll, dwords);
	fpadd(&tmp, &tmp, &p, dwords);
	fpdiv(pi, pi, &tmp, dwords);
	si2fp(&tmp, 8, dwords);
	fpsqr(&tmp, &tmp, dwords);
	fpdiv(pi, pi, &tmp, dwords);

	DecFloat_clear(&tmp2);
	DecFloat_clear(&tmp);
	DecFloat_clear(&r);
	DecFloat_clear(&q);
	DecFloat_clear(&p);
}

void pi_chudnovsky_binary_split(uint64_t a, uint64_t b, DecFloat_struct* pab, DecFloat_struct* qab, DecFloat_struct* tb, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;

	DecFloat_struct  c545140134, c13591409, temp, c3_over_24;
	uint64_t  m, t;

	DecFloat_init(&c545140134, NUM_DIGITS);
	DecFloat_init(&c13591409, NUM_DIGITS);
	DecFloat_init(&temp, NUM_DIGITS);
	DecFloat_init(&c3_over_24, NUM_DIGITS);

	si2fp(&c545140134, 545140134, dwords);
	si2fp(&c13591409, 13591409, dwords);
	c3_over_24.exponent = 1073741841;
	c3_over_24.mantissa[0] = 10939058;
	c3_over_24.mantissa[1] = 86003200;
	if ((b - a) == 1) {
		if (a == 0) {
			si2fp(pab, 1, dwords);
			si2fp(qab, 1, dwords);
		}
		else {
			t = 6 * a;
			si2fp(pab, t - 1, dwords);
			fpmul_si(pab, pab, a + a - 1, dwords);
			fpmul_si(pab, pab, t - 5, dwords);
			si2fp(&temp, a, dwords);
			fpmul(qab, &temp, &temp, dwords);
			fpmul(qab, qab, &temp, dwords);
			fpmul(qab, qab, &c3_over_24, dwords);
		}
		copydf(tb, &c545140134, dwords);
		fpmul_si(tb, tb, a, dwords);
		fpadd(tb, tb, &c13591409, dwords);
		fpmul(tb, tb, pab, dwords);
		if ((a % 2) != 0) {
			tb->sign = (tb->sign ^ (-1));
		}
	}
	else {
		DecFloat_struct  pam, qam, tam, pmb, qmb, tmb;

		DecFloat_init(&pam, NUM_DIGITS);
		DecFloat_init(&qam, NUM_DIGITS);
		DecFloat_init(&tam, NUM_DIGITS);
		DecFloat_init(&pmb, NUM_DIGITS);
		DecFloat_init(&qmb, NUM_DIGITS);
		DecFloat_init(&tmb, NUM_DIGITS);

		m = (a + b) / 2;
		pi_chudnovsky_binary_split(a, m, &pam, &qam, &tam, dwords);
		pi_chudnovsky_binary_split(m, b, &pmb, &qmb, &tmb, dwords);
		fpmul(pab, &pam, &pmb, dwords);
		fpmul(qab, &qam, &qmb, dwords);
		fpmul(&temp, &qmb, &tam, dwords);
		fpmul(tb, &pam, &tmb, dwords);
		fpadd(tb, tb, &temp, dwords);

		DecFloat_clear(&tmb);
		DecFloat_clear(&qmb);
		DecFloat_clear(&pmb);
		DecFloat_clear(&tam);
		DecFloat_clear(&qam);
		DecFloat_clear(&pam);
	}
	DecFloat_clear(&c3_over_24);
	DecFloat_clear(&temp);
	DecFloat_clear(&c13591409);
	DecFloat_clear(&c545140134);
}


void pi_chudnovsky_bs(DecFloat_struct* pi, int32_t digits) {
	int32_t dwords = (int32_t)(digits * 0.125 + 1);
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	DecFloat_struct  p, q, sqrtc, t, c3_over_24;
	uint64_t  n;
	double digits_per_term;

	DecFloat_init(&p, NUM_DIGITS);
	DecFloat_init(&q, NUM_DIGITS);
	DecFloat_init(&sqrtc, NUM_DIGITS);
	DecFloat_init(&t, NUM_DIGITS);
	DecFloat_init(&c3_over_24, NUM_DIGITS);

	digits_per_term = 14.18164746272548;
	c3_over_24.exponent = 1073741841;
	c3_over_24.mantissa[0] = 10939058;
	c3_over_24.mantissa[1] = 86003200;
	n = (uint64_t)floor(digits / digits_per_term + 1);
	pi_chudnovsky_binary_split(0, n, &p, &q, &t, dwords);
	si2fp(&sqrtc, 10005, dwords);
	fpsqr(&sqrtc, &sqrtc, dwords);
	fpmul_si(pi, &sqrtc, 426880, dwords);
	fpmul(pi, pi, &q, dwords);
	fpdiv(pi, pi, &t, dwords);

	DecFloat_clear(&c3_over_24);
	DecFloat_clear(&t);
	DecFloat_clear(&sqrtc);
	DecFloat_clear(&q);
	DecFloat_clear(&p);
}

void fplog_agm(DecFloat_struct* result, DecFloat_struct* x, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	int32_t     digits;
	int32_t     i;
	DecFloat_struct  one;
	DecFloat_struct  y;
	DecFloat_struct  tmp;

	DecFloat_init(&one, NUM_DIGITS);
	DecFloat_init(&y, NUM_DIGITS);
	DecFloat_init(&tmp, NUM_DIGITS);

	one.sign = 0;
	one.exponent = 0;
	for (i = 0; i <= dwords; i++)
	{
		one.mantissa[i] = 0;
	}
	if (fpcmp(x, &one, dwords) <= 0) {
		printf("%s\n", "can't take fplog_agm of zero or a negative number");
		DecFloat_clear(&tmp);
		DecFloat_clear(&y);
		DecFloat_clear(&one);
		return;
	}
	si2fp(&one, 1, dwords);
	if (fpcmp(x, &one, dwords) == 0) {
		for (i = 0; i <= dwords; i++)
		{
			result->mantissa[i] = 0;
		}
		result->sign = 0;
		result->exponent = 0;
		DecFloat_clear(&tmp);
		DecFloat_clear(&y);
		DecFloat_clear(&one);
		return;
	}
	if (pi_df.mantissa == 0) {
		DecFloat_init(&pi_df, NUM_DIGITS);
		pi_chudnovsky_bs(&pi_df, 8 * (NUM_DWORDS + EXTRAWORDS));
	}

	if (ln2_df.mantissa == 0) {
		DecFloat_init(&ln2_df, NUM_DIGITS);
		si2fp(&ln2_df, 2, NUM_DWORDS + EXTRAWORDS);
		fpipow(&ln2_df, &ln2_df, -2 * NUMBER_OF_DIGITS + 1, NUM_DWORDS + EXTRAWORDS);
		agm(&ln2_df, &one, &ln2_df, NUMBER_OF_DIGITS);
		fpmul_si(&ln2_df, &ln2_df, 2, NUM_DWORDS + EXTRAWORDS);
		fpdiv(&ln2_df, &pi_df, &ln2_df, NUM_DWORDS + EXTRAWORDS);
		fpdiv_si(&ln2_df, &ln2_df, 2 * NUMBER_OF_DIGITS + 1, NUM_DWORDS + EXTRAWORDS);
	}
	if (fpcmp(x, &one, dwords) < 0) {
		digits = (int32_t)(9.632959861247398 * dwords);
		si2fp(&y, 2, dwords);
		fpipow(&y, &y, 2 - 2 * digits, dwords);
		fpmul(&y, &y, x, dwords);
		agm(&y, &one, &y, digits);
		fpmul_si(&y, &y, 2, dwords);
		fpdiv(&y, &pi_df, &y, dwords);
		fpmul_si(&tmp, &ln2_df, 2 * digits, dwords);
		fpsub(result, &y, &tmp, dwords);
		result->sign = (-1);
		DecFloat_clear(&tmp);
		DecFloat_clear(&y);
		DecFloat_clear(&one);
	}
	else {
		digits = (int32_t)(9.632959861247398 * dwords);
		si2fp(&y, 2, dwords);
		fpipow(&y, &y, 2 - 2 * digits, dwords);
		fpdiv(&y, &y, x, dwords);
		agm(&y, &one, &y, digits);
		fpmul_si(&y, &y, 2, dwords);
		fpdiv(&y, &pi_df, &y, dwords);
		fpmul_si(&tmp, &ln2_df, 2 * digits, dwords);
		fpsub(result, &y, &tmp, dwords);

		DecFloat_clear(&tmp);
		DecFloat_clear(&y);
		DecFloat_clear(&one);
	}
}

void fpexp(DecFloat_struct* result, DecFloat_struct* x, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;

	DecFloat_struct  z, y;
	int32_t prec;
	if (dwords > 623) {
		prec = dwords / 4 + 1;

		DecFloat_init(&z, NUM_DIGITS);
		DecFloat_init(&y, NUM_DIGITS);
		fpexp_aux(&z, x, prec);

		prec *= 2;
		fplog_agm(&y, &z, prec);
		fpsub(&y, &y, x, prec);
		fpmul(&y, &y, &z, prec);
		fpsub(&z, &z, &y, prec);

		fplog_agm(&y, &z, dwords);
		fpsub(&y, &y, x, dwords);
		fpmul(&y, &y, &z, dwords);
		fpsub(result, &z, &y, dwords);

		DecFloat_clear(&y);
		DecFloat_clear(&z);
		return;
	}
	else {
		fpexp_aux(result, x, dwords);
	}
}

void get_pi_df(DecFloat_struct* result) {
	if (pi_df.mantissa == 0) {
		DecFloat_init(&pi_df, NUM_DIGITS);
		pi_chudnovsky_bs(&pi_df, 8 * (NUM_DWORDS + EXTRAWORDS));
	}
	copydf(result, &pi_df, NUM_DWORDS + EXTRAWORDS);
}

void get_pi2_df(DecFloat_struct* result) {
	if (pi2_df.mantissa == 0) {
		DecFloat_init(&pi2_df, NUM_DIGITS);
		if (pi_df.mantissa == 0) {
			DecFloat_init(&pi_df, NUM_DIGITS);
			pi_chudnovsky_bs(&pi_df, 8 * (NUM_DWORDS + EXTRAWORDS));
		}
		fpmul_si(&pi2_df, &pi_df, 2, NUM_DWORDS + EXTRAWORDS);
	}
	copydf(result, &pi2_df, NUM_DWORDS + EXTRAWORDS);
}

void get_pi_half_df(DecFloat_struct* result) {
	if (pi_half_df.mantissa == 0) {
		DecFloat_init(&pi_half_df, NUM_DIGITS);
		if (pi_df.mantissa == 0) {
			DecFloat_init(&pi_df, NUM_DIGITS);
			pi_chudnovsky_bs(&pi_df, 8 * (NUM_DWORDS + EXTRAWORDS));
		}
		fpdiv_si(&pi_half_df, &pi_df, 2, NUM_DWORDS + EXTRAWORDS);
	}
	copydf(result, &pi_half_df, NUM_DWORDS + EXTRAWORDS);
}

void get_ln2_df(DecFloat_struct* result) {
	if (ln2_df.mantissa == 0) {
		DecFloat_init(&ln2_df, NUM_DIGITS);
		if (pi_df.mantissa == 0) {
			DecFloat_init(&pi_df, NUM_DIGITS);
			pi_chudnovsky_bs(&pi_df, 8 * (NUM_DWORDS + EXTRAWORDS));
		}
		DecFloat_struct  one;
		DecFloat_init(&one, NUM_DIGITS);
		si2fp(&one, 1, NUM_DWORDS + EXTRAWORDS);
		si2fp(&ln2_df, 2, NUM_DWORDS + EXTRAWORDS);
		fpipow(&ln2_df, &ln2_df, -2 * NUMBER_OF_DIGITS + 1, NUM_DWORDS + EXTRAWORDS);
		agm(&ln2_df, &one, &ln2_df, NUMBER_OF_DIGITS);
		fpmul_si(&ln2_df, &ln2_df, 2, NUM_DWORDS + EXTRAWORDS);
		fpdiv(&ln2_df, &pi_df, &ln2_df, NUM_DWORDS + EXTRAWORDS);
		fpdiv_si(&ln2_df, &ln2_df, 2 * NUMBER_OF_DIGITS + 1, NUM_DWORDS + EXTRAWORDS);
		DecFloat_clear(&one);
	}
	copydf(result, &ln2_df, NUM_DWORDS + EXTRAWORDS);
}

void fplog10(DecFloat_struct* result, DecFloat_struct* x, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	fplog_agm(result, x, dwords);
	if (Log10e_df.mantissa == 0) {
		DecFloat_init(&Log10e_df, NUM_DIGITS);
		get_ln2_df(&Log10e_df);
		DecFloat_struct  one;
		DecFloat_init(&one, NUM_DIGITS);
		si2fp(&one, 1, NUM_DWORDS + EXTRAWORDS);
		fpdiv(&Log10e_df, &one, &ln10_df, NUM_DWORDS + EXTRAWORDS);
		DecFloat_clear(&one);
	}
	fpmul(result, result, &Log10e_df, dwords);
}

void get_ln10_df(DecFloat_struct* result) {
	if (ln10_df.mantissa == 0) {
		DecFloat_init(&ln10_df, NUM_DIGITS);
		if (pi_df.mantissa == 0) {
			DecFloat_init(&pi_df, NUM_DIGITS);
			pi_chudnovsky_bs(&pi_df, 8 * (NUM_DWORDS + EXTRAWORDS));
		}
		DecFloat_struct  one;
		DecFloat_struct  tmp;
		DecFloat_init(&one, NUM_DIGITS);
		DecFloat_init(&tmp, NUM_DIGITS);
		si2fp(&one, 1, NUM_DWORDS + EXTRAWORDS);
		if (ln2_df.mantissa == 0) {
			DecFloat_init(&ln2_df, NUM_DIGITS);
			si2fp(&ln2_df, 2, NUM_DWORDS + EXTRAWORDS);
			fpipow(&ln2_df, &ln2_df, -2 * NUMBER_OF_DIGITS + 1, NUM_DWORDS + EXTRAWORDS);
			agm(&ln2_df, &one, &ln2_df, NUMBER_OF_DIGITS);
			fpmul_si(&ln2_df, &ln2_df, 2, NUM_DWORDS + EXTRAWORDS);
			fpdiv(&ln2_df, &pi_df, &ln2_df, NUM_DWORDS + EXTRAWORDS);
			fpdiv_si(&ln2_df, &ln2_df, 2 * NUMBER_OF_DIGITS + 1, NUM_DWORDS + EXTRAWORDS);
		}
		si2fp(&ln10_df, 2, NUM_DWORDS + EXTRAWORDS);
		fpipow(&ln10_df, &ln10_df, 2 - 2 * NUMBER_OF_DIGITS, NUM_DWORDS + EXTRAWORDS);
		fpdiv_si(&ln10_df, &ln10_df, 10, NUM_DWORDS + EXTRAWORDS);
		agm(&ln10_df, &one, &ln10_df, NUMBER_OF_DIGITS);
		fpmul_si(&ln10_df, &ln10_df, 2, NUM_DWORDS + EXTRAWORDS);
		fpdiv(&ln10_df, &pi_df, &ln10_df, NUM_DWORDS + EXTRAWORDS);
		fpmul_si(&tmp, &ln2_df, 2 * NUMBER_OF_DIGITS, NUM_DWORDS + EXTRAWORDS);
		fpsub(&ln10_df, &ln10_df, &tmp, NUM_DWORDS + EXTRAWORDS);
		DecFloat_clear(&tmp);
		DecFloat_clear(&one);
	}
	copydf(result, &ln10_df, NUM_DWORDS + EXTRAWORDS);
}

void get_Log10e_df(DecFloat_struct* result) {
	if (Log10e_df.mantissa == 0) {
		DecFloat_init(&Log10e_df, NUM_DIGITS);
		get_ln2_df(&Log10e_df);
		DecFloat_struct  one;
		DecFloat_init(&one, NUM_DIGITS);
		si2fp(&one, 1, NUM_DWORDS + EXTRAWORDS);
		fpdiv(&Log10e_df, &one, &ln10_df, NUM_DWORDS + EXTRAWORDS);
		DecFloat_clear(&one);
	}
	copydf(result, &Log10e_df, NUM_DWORDS + EXTRAWORDS);
}

void get_exp1_df(DecFloat_struct* result) {
	if (exp1_df.mantissa == 0) {
		DecFloat_init(&exp1_df, NUM_DIGITS);
		DecFloat_struct  one;
		DecFloat_init(&one, NUM_DIGITS);
		si2fp(&one, 1, NUM_DWORDS + EXTRAWORDS);
		fpexp(&exp1_df, &one, NUM_DWORDS + EXTRAWORDS);
		DecFloat_clear(&one);
	}
	copydf(result, &exp1_df, NUM_DWORDS + EXTRAWORDS);
}

int32_t get_NUMBER_OF_DIGITS(void) {
	return NUMBER_OF_DIGITS;
}

int32_t get_NUM_BYTES(void) {
	return NUM_BYTES;
}

int32_t get_NUM_DWORDS(void) {
	return NUM_DWORDS;
}

void fppow(DecFloat_struct* result, DecFloat_struct* x, DecFloat_struct* y, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	fplog_agm(result, x, dwords);
	fpmul(result, result, y, dwords);
	fpexp(result, result, dwords);
}

void fpsin(DecFloat_struct* result, DecFloat_struct* x, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;

	DecFloat_struct xx, term, accum, p, temp2, fac, x_2;
	DecFloat_struct pi2, circ, ab;

	DecFloat_init(&xx, NUM_DIGITS);
	DecFloat_init(&term, NUM_DIGITS);
	DecFloat_init(&accum, NUM_DIGITS);
	DecFloat_init(&p, NUM_DIGITS);
	DecFloat_init(&temp2, NUM_DIGITS);
	DecFloat_init(&fac, NUM_DIGITS);
	DecFloat_init(&x_2, NUM_DIGITS);
	DecFloat_init(&pi2, NUM_DIGITS);
	DecFloat_init(&circ, NUM_DIGITS);
	DecFloat_init(&ab, NUM_DIGITS);

	//int32_t ex;

	//eps.exponent=-(dwords*9.63+20)+bias+1

	if (pi_df.mantissa == 0) {
		DecFloat_init(&pi_df, NUM_DIGITS);
		pi_chudnovsky_bs(&pi_df, 8 * (NUM_DWORDS + EXTRAWORDS));
	}
	copydf(&x_2, x, dwords);
	copydf(&pi2, &pi_df, dwords);
	fpmul_si(&circ, &pi_df, 2, dwords);
	copydf(&ab, x, dwords);
	ab.sign = 0;
	if (fpcmp(&ab, &circ, dwords) > 0) {
		//======== centralize ==============
		//floor/ceil to centralize
		DecFloat_struct tmp, tmp2;

		DecFloat_init(&tmp, NUM_DIGITS);
		DecFloat_init(&tmp2, NUM_DIGITS);
		fpmul_si(&pi2, &pi_df, 2, dwords);
		fpdiv(&tmp, &x_2, &pi2, dwords);
		fpfrac(&tmp, &tmp, dwords);
		fpmul(&x_2, &tmp, &pi2, dwords);
		DecFloat_clear(&tmp2);
		DecFloat_clear(&tmp);
	}

	int32_t lm, limit, i, lim = 0;
	DecFloat_struct factor;
	DecFloat_init(&factor, NUM_DIGITS);
	lm = (int32_t)(dwords * 9.63);
#if 1
	if (lm > 7000) {
		lim = lm;
		lm = 1000;
	}
#endif
	limit = (int32_t)(1 + floor(-.45344993886092585968 + (0.22333002852398072433e-1 + (5.0461814408333079844e-7 - 4.233845303980424e-11 * lm) * lm) * lm));
#if 1
	if (lim != 0) {
		limit = (int32_t)(lim / 6000 * limit);
	}
#endif
	if (limit < 0) limit = 0;
	si2fp(&factor, 5, dwords);
	fpipow(&factor, &factor, limit, dwords);
	fpdiv(&x_2, &x_2, &factor, dwords); //x_=x_/5^limit

	//==================================
	int32_t sign = -1;
	int32_t c = 1;

	copydf(&accum, &x_2, dwords);
	si2fp(&fac, 1, dwords);
	copydf(&p, &x_2, dwords);
	fpmul(&xx, &x_2, &x_2, dwords);

	do {
		c = c + 2;
		copydf(&temp2, &accum, dwords);
		fpmul_si(&fac, &fac, c * (c - 1), dwords);
		fpmul(&p, &p, &xx, dwords);
		fpdiv(&term, &p, &fac, dwords);
		if (sign < 0) {
			fpsub(&accum, &temp2, &term, dwords);
		}
		else {
			fpadd(&accum, &temp2, &term, dwords);
		}
		sign = -sign;
	} while (fpcmp(&accum, &temp2, dwords) != 0);

	//multiply the result by 5^limit

	for (i = 1; i <= limit; i++) {
		fpmul(&p, &accum, &accum, dwords);
		fpmul(&temp2, &accum, &p, dwords);
		//*** sin(5*x) = 5 * sin(x) - 20 * sin(x)^3 + 16 * sin(x)^5
		fpmul_si(&accum, &accum, 5, dwords);
		fpmul_si(&term, &temp2, 20, dwords);
		fpmul_si(&xx, &temp2, 16, dwords);
		fpmul(&xx, &xx, &p, dwords);
		fpsub(&accum, &accum, &term, dwords);
		fpadd(&accum, &accum, &xx, dwords);
	}
	copydf(result, &accum, dwords);

	DecFloat_clear(&factor);
	DecFloat_clear(&ab);
	DecFloat_clear(&circ);
	DecFloat_clear(&pi2);
	DecFloat_clear(&x_2);
	DecFloat_clear(&fac);
	DecFloat_clear(&temp2);
	DecFloat_clear(&p);
	DecFloat_clear(&accum);
	DecFloat_clear(&term);
	DecFloat_clear(&xx);
}

void fpcos(DecFloat_struct* result, DecFloat_struct* z, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	if (pi_half_df.mantissa == 0) {
		DecFloat_init(&pi_half_df, NUM_DIGITS);
		if (pi_df.mantissa == 0) {
			DecFloat_init(&pi_df, NUM_DIGITS);
			pi_chudnovsky_bs(&pi_df, 8 * (NUM_DWORDS + EXTRAWORDS));
		}
		fpdiv_si(&pi_half_df, &pi_df, 2, NUM_DWORDS + EXTRAWORDS);
	}
	fpsub(result, &pi_half_df, z, dwords);
	fpsin(result, result, dwords);

}

void fptan(DecFloat_struct* result, DecFloat_struct* z, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;

	DecFloat_struct s, c;
	DecFloat_init(&s, NUM_DIGITS);
	DecFloat_init(&c, NUM_DIGITS);
	fpsin(&s, z, dwords);
	fpcos(&c, z, dwords);
	fpdiv(result, &s, &c, dwords);
	DecFloat_clear(&c);
	DecFloat_clear(&s);
}

void fpatn(DecFloat_struct* result, DecFloat_struct* x, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	int32_t z, c;
	DecFloat_struct xx, term, accum, bigc, x2, mt, mt2, p;
	DecFloat_struct bignumber, bignumber2, one, factor, swap;

	DecFloat_init(&xx, NUM_DIGITS);
	DecFloat_init(&term, NUM_DIGITS);
	DecFloat_init(&accum, NUM_DIGITS);
	DecFloat_init(&bigc, NUM_DIGITS);
	DecFloat_init(&x2, NUM_DIGITS);
	DecFloat_init(&mt, NUM_DIGITS);
	DecFloat_init(&mt2, NUM_DIGITS);
	DecFloat_init(&p, NUM_DIGITS);
	DecFloat_init(&bignumber, NUM_DIGITS);
	DecFloat_init(&bignumber2, NUM_DIGITS);
	DecFloat_init(&one, NUM_DIGITS);
	DecFloat_init(&factor, NUM_DIGITS);
	DecFloat_init(&swap, NUM_DIGITS);

	copydf(&bignumber2, x, dwords);
	bignumber2.sign = 0;
	si2fp(&one, 1, dwords);
	if (pi_df.mantissa == 0) {
		DecFloat_init(&pi_df, NUM_DIGITS);
		pi_chudnovsky_bs(&pi_df, 8 * (NUM_DWORDS + EXTRAWORDS));
	}
	if (fpcmp(&bignumber2, &one, dwords) == 0) {
		fpdiv_si(&bignumber, &pi_df, 4, dwords);
		bignumber.sign = x->sign;
		copydf(result, &bignumber, dwords);
		DecFloat_clear(&swap);
		DecFloat_clear(&factor);
		DecFloat_clear(&one);
		DecFloat_clear(&bignumber2);
		DecFloat_clear(&bignumber);
		DecFloat_clear(&p);
		DecFloat_clear(&mt2);
		DecFloat_clear(&mt2);
		DecFloat_clear(&mt);
		DecFloat_clear(&x2);
		DecFloat_clear(&bigc);
		DecFloat_clear(&accum);
		DecFloat_clear(&term);
		DecFloat_clear(&xx);
		return;
	}
	bignumber2.sign = x->sign;
	int32_t limit; // = 16;

	limit = (int32_t)ceil(2 * (log(dwords * 8) * 0.6931471805599453));
	//si2fp(&factor, 2 << (limit - 1), dwords);
	si2fp(&factor, 2 << (limit - 1), dwords);

	for (z = 1; z <= limit; z++) {
		fpmul(&bignumber, &bignumber2, &bignumber2, dwords);
		fpadd(&bignumber, &bignumber, &one, dwords);
		fpsqr(&bignumber, &bignumber, dwords);
		fpadd(&bignumber, &bignumber, &one, dwords);
		fpdiv(&bignumber, &bignumber2, &bignumber, dwords);
		copydf(&bignumber2, &bignumber, dwords);
	}

	copydf(&mt, &bignumber, dwords);
	copydf(&x2, &bignumber, dwords);
	copydf(&p, &bignumber, dwords);
	fpmul(&xx, &x2, &x2, dwords);
	int32_t sign = -1;
	c = 1;
	for (;;) {
		c = c + 2;
		copydf(&mt2, &mt, dwords);
		si2fp(&bigc, c, dwords);
		fpmul(&p, &p, &xx, dwords);
		fpdiv(&term, &p, &bigc, dwords);
		if (sign < 0) {
			fpsub(&accum, &mt, &term, dwords);
		}
		else {
			fpadd(&accum, &mt, &term, dwords);
		}
		sign = -sign;
		copydf(&swap, &mt, dwords);
		copydf(&mt, &accum, dwords);
		copydf(&accum, &swap, dwords);
		if (fpcmp(&mt, &mt2, dwords) == 0) break;
	}
	fpmul(result, &factor, &mt, dwords);
	DecFloat_clear(&swap);
	DecFloat_clear(&factor);
	DecFloat_clear(&one);
	DecFloat_clear(&bignumber2);
	DecFloat_clear(&bignumber);
	DecFloat_clear(&p);
	DecFloat_clear(&mt2);
	DecFloat_clear(&mt2);
	DecFloat_clear(&mt);
	DecFloat_clear(&x2);
	DecFloat_clear(&bigc);
	DecFloat_clear(&accum);
	DecFloat_clear(&term);
	DecFloat_clear(&xx);
}

void fpasin(DecFloat_struct* result, DecFloat_struct* x, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	double num;
	DecFloat_struct one, t, b, term1, minusone;

	DecFloat_init(&one, NUM_DIGITS);
	DecFloat_init(&t, NUM_DIGITS);
	DecFloat_init(&b, NUM_DIGITS);
	DecFloat_init(&term1, NUM_DIGITS);
	DecFloat_init(&minusone, NUM_DIGITS);

	// arcsin = atn(x / sqr(-x * x + 1))
	//============= arcsin guard =========
	num = fp2dbl(x);

	if (num > 1) {
		copydf(result, &one, dwords);
		DecFloat_clear(&minusone);
		DecFloat_clear(&term1);
		DecFloat_clear(&b);
		DecFloat_clear(&t);
		DecFloat_clear(&one);
		return;
	}
	if (num < -1) {
		copydf(result, &one, dwords);
		DecFloat_clear(&minusone);
		DecFloat_clear(&term1);
		DecFloat_clear(&b);
		DecFloat_clear(&t);
		DecFloat_clear(&one);
		return;
	}
	//========================

	si2fp(&one, 1, dwords);
	si2fp(&minusone, -1, dwords);
	copydf(&t, x, dwords);
	fpmul(&b, x, x, dwords); //x*x
	//for 1 and -1
	if (pi_df.mantissa == 0) {
		DecFloat_init(&pi_df, NUM_DIGITS);
		pi_chudnovsky_bs(&pi_df, 8 * (NUM_DWORDS + EXTRAWORDS));
	}
	if (fpcmp(&b, &one, dwords) == 0) {
		DecFloat_struct two, atn1;
		DecFloat_init(&two, NUM_DIGITS);
		DecFloat_init(&atn1, NUM_DIGITS);
		si2fp(&two, 2, dwords);
		fpdiv_si(&atn1, &pi_df, 4, dwords);
		if (fpcmp(x, &minusone, dwords) == 0) {
			fpmul(&two, &two, &atn1, dwords);
			fpmul(result, &two, &minusone, dwords);
			DecFloat_clear(&atn1);
			DecFloat_clear(&two);
			return;
		}
		else {
			fpmul(result, &two, &atn1, dwords);
			DecFloat_clear(&atn1);
			DecFloat_clear(&two);
			return;
		}
	}
	fpsub(&b, &one, &b, dwords); //1-x*x
	fpsqr(&b, &b, dwords); //sqr(1-x*x)
	fpdiv(&term1, &t, &b, dwords);
	fpatn(result, &term1, dwords);
	DecFloat_clear(&minusone);
	DecFloat_clear(&term1);
	DecFloat_clear(&b);
	DecFloat_clear(&t);
	DecFloat_clear(&one);
}

void fpacos(DecFloat_struct* result, DecFloat_struct* x, int32_t dwords) {
	dwords += EXTRAWORDS;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	DecFloat_struct one, minusone, two, atn1, tail, t, b, term1, atnterm1; //,_x,temp
	double num;

	DecFloat_init(&one, NUM_DIGITS);
	DecFloat_init(&minusone, NUM_DIGITS);
	DecFloat_init(&two, NUM_DIGITS);
	DecFloat_init(&atn1, NUM_DIGITS);
	DecFloat_init(&tail, NUM_DIGITS);
	DecFloat_init(&t, NUM_DIGITS);
	DecFloat_init(&b, NUM_DIGITS);
	DecFloat_init(&term1, NUM_DIGITS);
	DecFloat_init(&atnterm1, NUM_DIGITS);

	//arccos = atn(-x / sqr(-x * x + 1)) + 2 * atn(1)
	//============= arccos guard =========
	num = fp2dbl(x);
	if (num > 1) {
		copydf(result, &one, dwords);
		DecFloat_clear(&atnterm1);
		DecFloat_clear(&term1);
		DecFloat_clear(&b);
		DecFloat_clear(&t);
		DecFloat_clear(&tail);
		DecFloat_clear(&atn1);
		DecFloat_clear(&two);
		DecFloat_clear(&minusone);
		DecFloat_clear(&one);
		return;
	}
	if (num < -1) {
		copydf(result, &one, dwords);
		DecFloat_clear(&atnterm1);
		DecFloat_clear(&term1);
		DecFloat_clear(&b);
		DecFloat_clear(&t);
		DecFloat_clear(&tail);
		DecFloat_clear(&atn1);
		DecFloat_clear(&two);
		DecFloat_clear(&minusone);
		DecFloat_clear(&one);
		return;
	}
	//========================
	if (pi_df.mantissa == 0) {
		DecFloat_init(&pi_df, NUM_DIGITS);
		pi_chudnovsky_bs(&pi_df, 8 * (NUM_DWORDS + EXTRAWORDS));
	}
	si2fp(&one, 1, dwords);
	si2fp(&minusone, -1, dwords);
	si2fp(&two, 2, dwords);
	fpdiv_si(&atn1, &pi_df, 4, dwords);
	fpmul(&tail, &two, &atn1, dwords); //2*atn(1)
	fpmul(&t, &minusone, x, dwords); //-x
	fpmul(&b, x, x, dwords); //x*x
	if (fpcmp(&b, &one, dwords) == 0) {
		//for 1 and -1
		if (fpcmp(x, &minusone, dwords) == 0) {
			copydf(result, &pi_df, dwords);
			DecFloat_clear(&atnterm1);
			DecFloat_clear(&term1);
			DecFloat_clear(&b);
			DecFloat_clear(&t);
			DecFloat_clear(&tail);
			DecFloat_clear(&atn1);
			DecFloat_clear(&two);
			DecFloat_clear(&minusone);
			DecFloat_clear(&one);
			return;
		}
		else {
			si2fp(result, 0, dwords);
			DecFloat_clear(&atnterm1);
			DecFloat_clear(&term1);
			DecFloat_clear(&b);
			DecFloat_clear(&t);
			DecFloat_clear(&tail);
			DecFloat_clear(&atn1);
			DecFloat_clear(&two);
			DecFloat_clear(&minusone);
			DecFloat_clear(&one);
			return;

		}
	}
	fpsub(&b, &one, &b, dwords); //1-x*x
	fpsqr(&b, &b, dwords); //sqr(1-x*x)
	fpdiv(&term1, &t, &b, dwords);
	fpatn(&atnterm1, &term1, dwords);
	fpadd(result, &atnterm1, &tail, dwords);
	DecFloat_clear(&atnterm1);
	DecFloat_clear(&term1);
	DecFloat_clear(&b);
	DecFloat_clear(&t);
	DecFloat_clear(&tail);
	DecFloat_clear(&atn1);
	DecFloat_clear(&two);
	DecFloat_clear(&minusone);
	DecFloat_clear(&one);
}

// the following string functions were borrowed from https://stackoverflow.com/a/1431206

char* ltrim(char* s)
{
	while (isspace(*s)) s++;
	return s;
}

char* rtrim(char* s)
{
	char* back;
	size_t len = strlen(s);

	if (len == 0)
		return(s);

	back = s + len;
	while (isspace(*--back));
	*(back + 1) = '\0';
	return s;
}

char* trim(char* s)
{
	return rtrim(ltrim(s));
}

// modified by me to trim 0's

char* ltrim0(char* s)
{
	while ((*s) == '0') s++;
	return s;
}

char* rtrim0(char* s)
{
	char* back;
	size_t len = strlen(s);

	if (len == 0)
		return(s);

	back = s + len;
	while ((*--back) == '0');
	*(back + 1) = '\0';
	return s;
}

char* rtrim0n(char* s, int32_t n)
{
	char* back;
	size_t len = strlen(s);

	if ((len == 0) || (n < 0))
		return(s);

	if (n == 0)
		return rtrim0(s);

	back = s + len;
	while ((*--back) == '0') {
		if (strlen(s) <= (n + 1)) break;
		*(back + 1) = '\0';
	}
	return s;
}

char* trim0(char* s)
{
	return rtrim0(ltrim0(s));
}

// my own string function

void string(char* s, int32_t n, char c)
{
	char* pbuff = s;
	int32_t i;
	for (i = 0; i < n; i++) {
		*pbuff = c;
		pbuff += 1;
	};
	*pbuff = '\0';
}

// tip for substring from https://stackoverflow.com/a/4214350

void str2fp(DecFloat_struct* result, char* value_in) {
	char* value = calloc(NUMBER_OF_DIGITS + 20, 1);
	char* buff = calloc(NUMBER_OF_DIGITS + 20, 1);
	char sn[10];
	int32_t i = 0, j = 0, k, sign = 0, ex = 0, lhs = 0, lz = 0, dp = 0;
	size_t len;
	unsigned char c;

	for (i = 0; i <= (NUM_DWORDS + EXTRAWORDS); i++) {
		result->mantissa[i] = '\0';
	}
	result->exponent = 0;
	result->sign = 0;
	i = 0;
	strncpy(value, trim(value_in), NUMBER_OF_DIGITS + 16);
	len = strlen(value);

	if (value[0] == '-') {
		sign = -1;
		i = 1;
	}
	if (value[0] == '+') {
		sign = 0;
		i = 1;
	}
	while (value[i] == '0') {
		i += 1;
	}
	while (i < len) {
		c = value[i++];

		if (c == ' ') {
			break;
		}

		else if ((c > 47) && (c < 58)) {
			buff[j] = c;
			j += 1;
			if (dp == 0) lhs += 1;
		}
		else if (c == '.') {
			if (dp == 1) {
				break;
			}
			else {
				dp = 1;
				if ((lhs == 0) && (i < len)) {
					while (value[i] == '0') {
						lz += 1;
						i += 1;
						if (i >= len) break;
					}
				}
			}
		}
		else if (c == 'e' || c == 'E') {
			ex = atoi(&value[i]);
			break;
		}
		else {
			break;
		}

	}

	if (lhs == 0) {
		ex = ex - lz - 1;
	}
	else {
		ex = ex + lhs - 1;
	}
	len = strlen(buff);
	if (len > 0) {
		k = len % 8;
		if (k) {
			for (i = 0; i < (8 - k); i++) {
				buff[j++] = '0';
			}
		}
	}

	len = strlen(buff);

	if (len != 0) {
		i = 0;
		j = 0;
		while (i < len) {
			memcpy(sn, buff + i, 8);
			sn[8] = '\0';
			result->mantissa[j] = atol(sn);
			i += 8;
			j += 1;
		}
		result->exponent = ex + BIAS + 1;
		result->sign = sign;
	}
	free(buff);
	free(value);

}

void fp2str_exp(char* result, DecFloat_struct* z, int32_t digits) {
	//if (digits > NUMBER_OF_DIGITS) digits = NUMBER_OF_DIGITS;
	if (digits > (NUMBER_OF_DIGITS + EXTRAWORDS * 8 + 8)) digits = NUMBER_OF_DIGITS + EXTRAWORDS * 8 + 8;
	int32_t ex, i, dwords, zsign;
	uint32_t xpz;
	size_t len;
	char sn[16];
	DecFloat_struct n;
	DecFloat_init(&n, NUM_DIGITS);
	len = strlen(result);

	dwords = digits / 8;
	if ((8 * dwords) < digits) dwords += 1;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	for (i = 0; i < len; i++) {
		result[i] = 0;
	}
	if (z->exponent != 0) {
		ex = (z->exponent & 0x7fffffff) - BIAS - 1;
	}
	else {
		ex = 0;
	}
	xpz = z->exponent;
	z->exponent = digits + BIAS + 1;
	zsign = z->sign;
	z->sign = 0;
	si2fp(&n, 5, dwords);
	fpadd(&n, &n, z, dwords);
	n.exponent = digits + BIAS;
	fpfix(&n, &n, dwords);
	n.exponent = xpz;
	n.sign = zsign;
	z->exponent = xpz;
	z->sign = zsign;
	if (n.sign < 0) {
		sn[0] = '-';
	}
	else {
		sn[0] = ' ';
	}
	sn[1] = '\0';
	strcat(result, (char*)&sn);
	sprintf((char*)&sn, "%d%s%07d", n.mantissa[0] / 10000000, ".", n.mantissa[0] % 10000000);
	strcat(result, (char*)&sn);
	for (i = 1; i < (dwords); i++) {
		sprintf((char*)&sn, "%08d", n.mantissa[i]);
		strcat(result, (char*)&sn);
	}
	result = trim0(result);

	len = strlen(result) - 1;
	if (*(result + len) == '.') *(result + len) = '\0';
	if (ex != 0) {
		sprintf((char*)&sn, "%s%d", "e", ex);
		strcat(result, (char*)&sn);
	}
	DecFloat_clear(&n);

}

void fp2str(char* result, DecFloat_struct* z, int32_t digits) {
	if (digits > (NUMBER_OF_DIGITS + EXTRAWORDS * 8 + 8)) digits = NUMBER_OF_DIGITS + EXTRAWORDS * 8 + 8;
	int32_t e, ex, i, dwords, zsign;
	if (z->exponent != 0) {
		ex = (z->exponent & 0x7fffffff) - BIAS - 1;
	}
	else {
		ex = 0;
	}
	if (z->exponent == 0) {
		strcpy(result, " 0");
		return;
	}
	if (ex <= (-5) || (ex > digits)) {
		fp2str_exp(result, z, digits);
	}
	else {
		uint32_t xpz;
		size_t len;
		char sn[16];
		char* v = calloc(digits + 20, 1);
		DecFloat_struct n;
		DecFloat_init(&n, NUM_DIGITS);
		len = strlen(result);

		dwords = digits / 8;
		if ((8 * dwords) < digits) dwords += 1;
		if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
		for (i = 0; i < len; i++) {
			result[i] = 0;
		}
		e = abs(ex);
		if (e > digits) {
			fp2str_exp(result, z, digits);
			DecFloat_clear(&n);
			free(v);
			return;
		}
		xpz = z->exponent;
		z->exponent = digits + BIAS + 1;
		zsign = z->sign;
		z->sign = 0;
		si2fp(&n, 5, dwords);
		fpadd(&n, &n, z, dwords);
		n.exponent = digits + BIAS;
		fpfix(&n, &n, dwords);
		n.exponent = xpz;
		n.sign = zsign;
		z->exponent = xpz;
		z->sign = zsign;

		for (i = 0; i < (dwords); i++) {
			sprintf((char*)&sn, "%08d", n.mantissa[i]);
			strcat(result, (char*)&sn);
		}
		result = trim0(result);
		if (strlen(result) <= e) {
			string(v, e, '0');
			strcat(result, v);
		}

		if (e == 0) {
			if (n.sign < 0) {
				sn[0] = '-';
			}
			else {
				sn[0] = ' ';
			}
			memcpy((char*)&sn + 1, result, 1);
			sn[2] = '.';
			sn[3] = '\0';
			memcpy(v, result + 1, digits);
			*(v + digits) = '\0';
			strcpy(result, (char*)&sn);
			strcat(result, v);
		}
		else if (ex > (-5) && (ex < 0)) {
			if (n.sign < 0) {
				sn[0] = '-';
			}
			else {
				sn[0] = ' ';
			}
			sn[1] = '0';
			sn[2] = '.';
			for (i = 1; i < e; i++) {
				sn[i + 2] = '0';
			}
			sn[i + 2] = '\0';
			strcpy(v, sn);
			strcat(v, (result));
			strcpy(result, v);
		}
		else if (ex > 0) {
			if (n.sign < 0) {
				sn[0] = '-';
			}
			else {
				sn[0] = ' ';
			}
			sn[1] = '\0';
			strcpy(v, sn);
			memcpy((v + 1), (result), e + 1);
			if ((e < (digits - 1))) {
				strcat(v, ".");
			}
			strcat(v, (result + e + 1));
			strcpy(result, v);

		}
		len = strlen(result) - 1;
		if (*(result + len) == '.') *(result + len) = '\0';
		
		result = trim0(result);
		DecFloat_clear(&n);
		free(v);
	}

}

void printdf(DecFloat_struct* z, int32_t dwords) {
	if (dwords > NUM_DWORDS + EXTRAWORDS) dwords = NUM_DWORDS + EXTRAWORDS;
	int32_t ex, i;
	if (z->exponent != 0) {
		ex = (z->exponent & 0x7fffffff) - BIAS - 1;
	}
	else {
		ex = 0;
	}
	if (z->sign < 0) printf("%s", "-");
	printf("%d%s%07d", z->mantissa[0] / 10000000, ".", z->mantissa[0] % 10000000);

	for (i = 1; i <= dwords; i++) {
		printf("%08d", z->mantissa[i]);
	}
	printf("%s%d\n", "e", ex);
}

void fp2str2(char* result, DecFloat_struct* z, int32_t* exz, int32_t digits) {
	if (digits > (NUMBER_OF_DIGITS)) digits = NUMBER_OF_DIGITS;
	int32_t i, dwords;
	size_t len;
	char sn[16];

	if (z->exponent == 0) {
		strncpy(result, "0", digits + 16);
		*(result + 1) = 0;
		return;
	}
	if (z->exponent != 0) {
		*exz = (int32_t)(z->exponent & 0x7fffffff) - BIAS - 1;
	}
	else {
		*exz = 0;
	}

	dwords = digits / 8;
	if ((8 * dwords) < digits) dwords += 1;
	if (dwords > (NUM_DWORDS + EXTRAWORDS)) dwords = NUM_DWORDS + EXTRAWORDS;
	len = strlen(result);
	for (i = 0; i < len; i++) {
		result[i] = 0;
	}

	for (i = 0; i <= (dwords); i++) {
		sprintf((char*)&sn, "%08d", z->mantissa[i]);
		strcat(result, (char*)&sn);
	}
	result = trim0(result);
}

