#Include "BigFloat_bitnormalized.bas"

Sub BigFloat_Exp(Byref Result As BigFloat, Byref X As BigFloat)

    Dim As BigFloat xx
    Dim As BigFloat q
    Dim As BigFloat qround
    Dim As BigFloat half
    Dim As BigFloat kfp
    Dim As BigFloat r

    Dim As BigFloat term
    Dim As BigFloat sum
    Dim As BigFloat oldsum
    Dim As BigFloat tmp

    Dim As Longint k
    Dim As Longint newExp
    Dim As Ulongint n
    Dim As Long i, maxn = Clng((SIG_LEN * 2.4082399653118495))

    ' ----------------------------------------------------------
    ' Special cases
    ' ----------------------------------------------------------
    If IsNaN_F(X) Then
        BigFloat_SetNaN Result
        Exit Sub
    End If

    If IsZero_F(X) Then
        BigFloat_FromLong Result, 1
        Exit Sub
    End If

    ' Alias safety.
    BigFloat_Assign xx, X


    ' ----------------------------------------------------------
    ' q = x / ln(2)
    ' ----------------------------------------------------------
    BigFloat_Divide q, xx, fpln2.BigNum


    ' ----------------------------------------------------------
    ' k = nearest integer(x / ln(2))
    ' ----------------------------------------------------------
    BigFloat_Pow2 half, -1      ' 0.5 exactly

    If IsNeg_F(q) Then
        BigFloat_Subtract qround, q, half
    Else
        BigFloat_Add qround, q, half
    End If

    BigFloat_Truncate qround, qround

    If BigFloat_ToLongInt(qround, k) = False Then

        If IsNeg_F(xx) Then
            BigFloat_Clear Result
        Else
            BigFloat_SetNaN Result
        End If

        Exit Sub
    End If


    ' ----------------------------------------------------------
    ' r = x - k*ln(2)
    '
    ' Therefore:
    '
    '     |r| <= ln(2)/2
    ' ----------------------------------------------------------
    BigFloat_FromLongInt kfp, k

    BigFloat_Multiply tmp, fpln2.BigNum, kfp
    BigFloat_Subtract r, xx, tmp


    ' ----------------------------------------------------------
    ' Additional range reduction:
    '
    '     exp(r) = exp(r/16)^16
    '
    ' Since 16 = 2^4:
    '
    '     exp(r) =
    '         ((((exp(r/16))^2)^2)^2)^2)
    '
    ' Now:
    '
    '     |r/16| <= ln(2)/32
    '            ~= 0.02166084939
    '
    ' which makes the Taylor series converge very rapidly.
    ' ----------------------------------------------------------

    r.exponent -= 4


    ' ----------------------------------------------------------
    ' Taylor series for exp(r/16)
    '
    '     sum  = 1
    '     term = 1
    '
    '     term(n) = term(n-1) * r / n
    '
    ' Stop when the next term no longer changes the rounded
    ' BigFloat result.
    ' ----------------------------------------------------------
    BigFloat_FromLong sum, 1
    BigFloat_FromLong term, 1

    n = 1

    Do

        BigFloat_Multiply tmp, term, r
        BigFloat_DivideU64 term, tmp, n

        BigFloat_Assign oldsum, sum
        BigFloat_Add sum, sum, term

        If BigFloat_Compare(sum, oldsum) = 0 Then Exit Do

        n += 1

        ' Defensive limit only.
        If n > maxn Then Exit Do

    Loop

    ' ----------------------------------------------------------
    ' Undo reduction:
    '
    '     exp(r) = exp(r/16)^16
    '
    ' Four squarings.
    ' ----------------------------------------------------------
    For i = 1 To 4
        BigFloat_Multiply tmp, sum, sum
        BigFloat_Assign sum, tmp
    Next


    ' ----------------------------------------------------------
    ' exp(x) = exp(r) * 2^k
    '
    ' Multiplication by 2^k is exact in the new representation.
    ' ----------------------------------------------------------
    newExp = Clngint(sum.exponent) + k

    If newExp > 2147483647LL Then
        BigFloat_SetNaN Result
        Exit Sub
    End If

    If newExp < -2147483648LL Then
        BigFloat_Clear Result
        Exit Sub
    End If

    BigFloat_Assign Result, sum
    Result.exponent = Clng(newExp)

End Sub

Function fpExp(Byref x As BigFloat_t) As BigFloat_t

    Dim As BigFloat_t result

    BigFloat_Exp result.BigNum, x.BigNum

    Return result

End Function

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

Function F(X As BigFloat_t) As BigFloat_t
	Return fpExp(X)
End Function
 
Function LegInt(A As BigFloat_t, B As BigFloat_t) As BigFloat_t
	Dim As Long I
	Dim As BigFloat_t C1, C2, Result

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
	Return Left(s, 42)+se
End Function
		
Dim As BigFloat_t x, y, z
Dim As Long i, c
Dim As String s, se
	gauss_leg_rule(N, Xi(), Wi())
	Print "   Gauss-Legendre degree ";N;" Quuadrature rule"
	Print "                    Xi                                             Wi"
	For i=1 To N
		Print strn(Xi(i), 42), strn(Wi(i), 42)
	Next
	
z=LegInt(BigFloat_t(-3), BigFloat_t(3))
Print
Print "Integrating Exp(x) over [-3, 3] = ";strn(z, 42)
x=3
y=fpexp(x)-fpexp(-x)
Print "Actual value = ";strn(y, 42)
z=(z-y)/y
Print "relative error = ";strn(z, 42)

