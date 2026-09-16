#Include "BigFloat_bitnormalized.bas"

Sub gauss_leg_rule(Byval n As Const Long, x() As BigFloat_t,  w() As BigFloat_t)
	Dim As Long m, j, i, num_digits = Clng(SIG_LEN * 9.632959861247398) ' number of 32-bit limbs * 32 * log10(2)
	Dim As BigFloat_t eps, x1, x2, z1, z, xm, xl, pp, p3, p2, p1
	Dim As String s

	s=Str(num_digits-5)
	s=Trim(s)
	eps="1e-"+s
	x1=-1
	x2=1
	m = (n+1) \ 2
	xm = 0.5*(x2+x1)
	xl = 0.5*(x2-x1)
	For i = 1 To m
		z = Cos(3.1415926535897932*(i-0.25)/(n+0.5))
		Do
			p1 = 1
			p2 = 0
			For j = 1 To n
				p3 = p2
				p2 = p1
				p1 = ((2*j-1)*z*p2-(j-1)*p3)/j
			Next
			pp = n*(z*p1-p2)/(z*z-1)
			z1 = z
			z = z1-p1/pp
		Loop Until (Abs(z-z1) <= eps)
		x(i) = xm-xl*z
		x(n+1-i) = xm+xl*z
		w(i) = 2*xl/((1-z*z)*pp*pp)
		w(n+1-i) = w(i)
	Next
End Sub

Const N As Long = 16
Dim Shared As BigFloat_t Xi(N)
Dim Shared As BigFloat_t Wi(N)

Function LegInt(fun as function (X As BigFloat_t) As BigFloat_t, A As BigFloat_t, B As BigFloat_t) As BigFloat_t
	Dim As Long I
	Dim As BigFloat_t C1, C2, Result
	Dim f as function (X As BigFloat_t) As BigFloat_t = Fun
	
	C1 = (B-A)/2
	C2 = (B+A)/2
	Result = 0
	For I = 1 To N
		Result = Result + Wi(I) * F(C1*XI(I) + C2)
	Next
	Result = C1 * Result
	Return Result
End Function

Function strn(x As BigFloat_t, Byval digits As Long) As String
	Dim As Long c
	Dim As String s, se

	se=""
	s=x
	c = Instr(s, "e")
	If c>0 Then
		se=Mid(s, c)
	End If
	Return Left(s, digits)+se
End Function

Function Func1(X As BigFloat_t) As BigFloat_t
	Return fpExp(X)
End Function

Function Func2(X As BigFloat_t) As BigFloat_t
	Return 1/(X * X + 1)
End Function

Function Func3(X As BigFloat_t) As BigFloat_t
	Return 1/fpSqr(1 - X * X)
End Function

Dim As BigFloat_t x, y, z
Dim As Long i, c
Dim fn as function (X As BigFloat_t) As BigFloat_t

gauss_leg_rule(N, Xi(), Wi())
Print "   Gauss-Legendre degree ";N;" Quuadrature rule"
Print "                    Xi                                             Wi"
For i=1 To N
	Print strn(Xi(i), 42), strn(Wi(i), 42)
Next

fn = @Func1
z=LegInt(fn, BigFloat_t(-3), BigFloat_t(3))

Print
Print "Integrating Exp(x) over [-3, 3] = ";strn(z, 42)
x=3
y=fn(x)-fn(-x)
Print "Actual value = ";strn(y, 42)
z=(z-y)/y
Print "relative error = ";strn(z, 42)
z=LegInt(fn, BigFloat_t(-3), BigFloat_t(3))

Print
fn = @Func2
z=LegInt(fn, BigFloat_t(0), BigFloat_t(1))
Print "Integrating 1/(1+x^2) over [0, 1] = ";strn(z, 42)
y=fppi/4
Print "Actual value = ";strn(y, 42)
z=(z-y)/y
Print "relative error = ";strn(z, 42)

Print
fn = @Func3
z=LegInt(fn, BigFloat_t(0), BigFloat_t(.5))
Print "Integrating 1/Sqr(1-x^2) over [0, .5] = ";strn(z, 42)
y=fppi/6
Print "Actual value = ";strn(y, 42)
z=(z-y)/y
Print "relative error = ";strn(z, 42)
