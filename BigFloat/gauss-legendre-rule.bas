Const NUMBER_OF_DIGITS = 64
#Include "BigFloat-b.bi"

Const order=12
Dim Shared As bigfloat z1, z2, eps, roots(order), weights(order), legcoef(order, order)
Dim As Double tm, t
dim as integer i, ex, n

eps.BigNum.exponent=-(NUM_BITS-16)+BIAS+1
eps.BigNum.mantissa[0]=&h10000000ul

Function f(Byref x As bigfloat) As bigfloat
	Return Exp(x)
End Function

Sub prepcoef
	Dim As Integer i, n

	For i=0 To order
		For n = 0 To order
			legcoef(i,n) = 0
		Next
	Next
	legcoef(0,0) = 1
	legcoef(1,1) = 1
	For n=2 To order
		legcoef(n,0) = -(n-1) * legcoef(n-2,0) / n
		For i = 1 To order
			legcoef(n,i) = ((2*n-1) * legcoef(n-1,i-1) - (n-1)*legcoef(n-2,i)) / n
		Next
	Next
End Sub

Function legeval(Byval n As Integer, Byref x As bigfloat) As bigfloat
	Dim As Integer i
	Dim As bigfloat result

	result = legcoef(n,n)
	For i = n-1 To 0 Step -1
		result = result * x + legcoef(n,i)
	Next
	Return result
End Function

Function legdiff(Byval n As Integer, Byref x As bigfloat) As bigfloat
	Return n * (x * legeval(n,x) - legeval(n-1,x)) / (x*x-1)
End Function

Sub legroots
	Dim As Integer i
	Dim As bigfloat x, x1

	For i = 1 To order
		x = Cos(pi_bf * (i-bigfloat(0.25)) / (order+bigfloat(0.5)))
		Do
			x1 = x
			x = x - legeval(order,x) / legdiff(order, x)
			if Abs (x)>Abs(x1) then
				x=x1
				exit do
			end if
		Loop Until Abs (x-x1) < eps
		roots(i-1) = x
		x1 = legdiff(order,x)
		weights(i-1) = bigfloat(2) / ((1-x*x) * x1*x1)
	Next
End Sub

Function legint(Byref a As bigfloat, Byref b As bigfloat) As bigfloat
	Dim As Integer i
	Dim As bigfloat c1, c2, result

	c1 = (b-a)/2
	c2 = (b+a)/2
	result = 0
	For i = 0 To order-1
		result = result + weights(i) * f(c1*roots(i) + c2)
	Next
	result = c1 * result
	Return result
End Function
tm=timer
prepcoef
legroots
tm=timer-tm

z1=0
For i=0 To order-1
	z1+=roots(i)
Next

if z1.BigNum.exponent<>0 then
	t=(z1.BigNum.exponent And &h7FFFFFFF)-BIAS-1
else
	t=0
endif
t*=0.3010299956639812 'multiply binary exponent by log10(2) to get decimal exponent
ex=int(abs(t))

if ex=0 or ex>NUMBER_OF_DIGITS then ex=NUMBER_OF_DIGITS
if ex>73 then ex=73
Print "Abscissas (Zeros of Legendre Polynomials) Xi"
For i=0 To order-1
	Print roots(i).toString(ex-3)
Next
Print
Print string(ex+6, "=")
Print
z2=0
Print "the Weight Factors Wi"
For i=0 To order-1
	Print weights(i).toString(ex-3)
	z2+=weights(i)
Next
Print
Print "integrating exp(x) over [-3, 3]: ";(legint(-3,3)).toString(37)
Print "actual value:                    ";(Exp(bigfloat(3))-Exp(bigfloat(-3))).toString(37)
Print "elapsed time"; tm
Print
Print "the sum of the roots should be close to 0:   "; z1.toString(20)
Print "the sum of the weights should be close to 2: "; z2.toString(20)
Print "                                   2 - sum = ";(2-z2).toString(20)
Print "digits in output is "; ex-3
