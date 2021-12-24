Const NUMBER_OF_DIGITS = 2.63*14+20 
#Include "BigFloat.bi"

declare Sub gauss_jordan ( a() As BigFloat, y() As BigFloat, coef() As BigFloat, ByVal ncol As Long, ByRef error_code As Long)
declare sub make_lanczos(x() As BigFloat, byval g as string, byref ercode as long)
declare sub gamma_lanczos( byval n as integer, byref g as string, byval digits_prec as long)
dim shared as BigFloat e, pi, zero, one, half

gamma_lanczos(6, "5", 20)
'print "=================================="
'gamma_lanczos(14, "4.7421875",20)
dim as double t=timer
gamma_lanczos(13, "6.024680040776729583740234375", 37)
t=timer-t
? t
'print "press any key to exit ";
'sleep
end
'===============================================================================
sub gamma_lanczos( byval n as integer = 6, byref g as string = "5", byval digits_prec as long=37)

dim as integer i
dim as long error_code
dim as BigFloat x(n)
Dim As string frmt, str_out

zero=0
one=1
half=".5"
e=exp(one)
pi=pi_bf

make_lanczos(x(), g, error_code)

if not error_code then
	'open "lanczos_gamma_"+str(n)+".txt" for output as 1
	For i=0 To n
		str_out=x(i).toString(digits_prec)
		frmt=str(i)
		if i<10 then frmt=" "+frmt
		str_out="C("+frmt+") = "+str_out
		Print str_out
		'Print #1, str_out
	Next
	'close 1
	print
end if

end sub

sub make_lanczos(x() As BigFloat, byval g_ as string, byref error_code as long)
	
	Dim As Long i, j
	Dim As Integer sp
	dim as integer n = ubound(x)
	Dim a(n, n) As BigFloat 
	Dim b(n) As BigFloat
	Dim As BigFloat g, sqr2pi, pi2, fac
	
	pi2=pi*2
	sqr2pi=sqr( pi2)
	g=g_
	
	'setup the matrix, for n=6 the matrix looks like this
	
	' [[ 1,	1,	 1/2, 1/3, 1/4,  1/5,  1/6  ] 
	' [  1,	1/2, 1/3, 1/4, 1/5,  1/6,  1/7  ] 
	' [  1,	1/3, 1/4, 1/5, 1/6,  1/7,  1/8  ] 
	' [  1,	1/4, 1/5, 1/6, 1/7,  1/8,  1/9  ] 
	' [  1.	1/5, 1/6, 1/7, 1/8,  1/9,  1/10 ] 
	' [  1,	1/6, 1/7, 1/8, 1/9,  1/10, 1/11 ]
	' [  1,	1/7, 1/8, 1/9, 1/10, 1/11, 1/12 ]] 

	For i=0 To n
	  a(i,0)=1
	  For j=1 To n
		a(i,j)=one/(i+j)
	  Next
	Next

	'setup the constant terms, for n=6 and g=5 they are
	'b( 0) = 41.62443691643906820752
	'b( 1) = 16.01231640525168135809
	'b( 2) =  9.36473553710404851170
	'b( 3) =  6.57049625931606149987
	'b( 4) =  5.09521672929646781312
	'b( 5) =  4.20380175313001102196
	'b( 6) =  3.61487381446333490266
	
	fac=1
	
	For i=0 To n
		if i>1 then
			fac=fac*i
		end if
		b(i)=exp(i+half+g)/(i+half+g)^(i+half)*fac/sqr2pi
		'print b(i).toString("%25.20f")
	Next

	'now solve the system of linear equations, for n=6 and g=5 they are
	' [[ 1,	1,	 1/2, 1/3, 1/4,  1/5,  1/6  ]	= 41.62443691643906820752
	' [  1,	1/2, 1/3, 1/4, 1/5,  1/6,  1/7  ]	= 16.01231640525168135809
	' [  1,	1/3, 1/4, 1/5, 1/6,  1/7,  1/8  ]	=  9.36473553710404851170
	' [  1,	1/4, 1/5, 1/6, 1/7,  1/8,  1/9  ]	=  6.57049625931606149987
	' [  1.	1/5, 1/6, 1/7, 1/8,  1/9,  1/10 ]	=  5.09521672929646781312 
	' [  1,	1/6, 1/7, 1/8, 1/9,  1/10, 1/11 ]	=  4.20380175313001102196
	' [  1,	1/7, 1/8, 1/9, 1/10, 1/11, 1/12 ]]	=  3.61487381446333490266
	
	'and the solution for the above example is
	'C(0) =	  1.00000000019001482399
	'C(1) =	 76.18009172947146348309
	'C(2) =	-86.50532032941676765250
	'C(3) =	 24.01409824083091049018
	'C(4) =	 -1.23173957245015538752
	'C(5) =	  0.00120865097386617851
	'C(6) =	 -0.00000539523938495313
	
	'which is the Lanczos gamma function approximation
	'as published in Numerical recipes

	gauss_jordan(a(), b(), x(), n, error_code)
end sub

Sub gauss_jordan ( a() As BigFloat, y() As BigFloat, coef() As BigFloat, ByVal ncol As Long, ByRef error_code As Long)

	' matrix solution by Gaussian Elimination

	Dim As BigFloat b(ncol, ncol), w(ncol)				' work array, ncol long

	Dim As Long i,j,i1,k,l,n
	Dim As BigFloat hold, sm, t, ab, big
'	Const TRUE = -1
'	Const FALSE = Not TRUE

	error_code=FALSE
	n=ncol
	
	For i=0 To n
		' copy to work arrays
		For j=0 To n
			b(i,j)=a(i,j)
		Next j
		w(i)=y(i)
		coef(i)=zero
	Next
	For i=0 To n-1
		big=abs(b(i,i))
		l=i
		i1=i+1
		For j=i1 To n
			' search for largest element	
			ab=abs(b(j,i))
			If ab>big Then
				big=ab
				l=j
			End If
		Next
		If big=zero Then
			error_code= TRUE
		Else
			If l<>i Then
				' interchange rows to put largest element on diagonal
				For j=0 To n
					hold=b(l,j)
					b(l,j)=b(i,j)
					b(i,j)=hold
				Next
				hold=w(l)
				w(l)=w(i)
				w(i)=hold
			End If
			For j=i1 To n
				t=b(j,i)/b(i,i)
				For k=i1 To n
					b(j,k)=b(j,k)-t*b(i,k)
				Next
				w(j)=w(j)-t*w(i)
			Next	' j-loop
		End If	' if big - else
	Next			' i-loop
	If b(n,n)=zero Then
		error_code=TRUE
	Else
		coef(n)=w(n)/b(n,n)
		i=n-1
		' back substitution
		Do
			Sm=zero
			For j=i To n
				sm=sm+b(i,j)*coef(j)
			Next
			coef(i)=(w(i)-sm)/b(i,i)
			i=i-1
		Loop Until i<0
	End If
	If error_code Then Print "ERROR: Matrix is singular" 
End Sub
