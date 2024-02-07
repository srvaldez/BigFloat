const dec_float_digits_precision = 10000

#Include "DecFloatC.bi"

Dim As Decfloat x, y, z, s
Dim As Double t
dim as long i

x="8."+string(NUMBER_OF_DIGITS, "8")
y="."+string(NUMBER_OF_DIGITS, "8")
t=timer
s=0

for i=1 to 100
	x=i
	s=s+sqr(x)
next

t=timer-t
? s
? t
