
#include once "quadmath.bi"

dim as __float128 x, y, z
dim as string s

? " x             Sqrtq(x)                                  relative error"
for x=1 to 20
	y = sqrtq(x)
	z = (y^2-x)/x 'relative error
	s=y.toString
	if len(s)<48 then
		s=s+space(43-len(s))
	end if
	s+=z.toString
	? flt128_i32(x), s
next

? "press return to end"
sleep
