Const NUMBER_OF_DIGITS = 80
#Include "BigFloat.bi"

Function fib_mod(Byref k As BigFloat, Byref m As BigFloat) As BigFloat
	Dim As BigFloat b=k, x, y, xx, temp, p2, rn
	Dim As Integer i, bit_length, c
	Dim As String a, s
	If k <= 1 Then Return k
	x = 1: y = 0
	bit_length=Fix(Log(k)/Log(BigFloat(2))+".05")
	p2=2
	p2.BigNum=fpipow(p2.BigNum, bit_length-1)
	rn="0.05"
	For i = bit_length-1 To 0 Step -1
		'xx = x*x Mod m
		xx = (x*x)/m
		xx = Frac(xx) :xx=xx*m+rn : xx.BigNum=fpfix(xx.BigNum)
		x = (xx + 2*x*y)/m
		x=Frac(x)*m +rn: x.BigNum=fpfix(x.BigNum)
		'y = (xx + y*y) Mod m
		y = (xx + y*y)/m
		y=Frac(y)*m +rn: y.BigNum=fpfix(y.BigNum)
		temp=b/p2
		'If Bit(k,i) Then
		If fpfix_is_odd(temp.BigNum) Then
			temp = x
			'x = (x + y) Mod m
			x = (x + y)/m
			x=Frac(x)*m +rn: x.BigNum=fpfix(x.BigNum)
			y = temp
		End If
		p2=Fix(p2/2)
	Next
	Return x+rn
End Function

Dim As BigFloat last9, fn, m
Dim As Double t
Dim As Long dp, xp, show_number_of_digits
Dim As String a, s, sn, sl
Dim As BigFloat x, y, z

show_number_of_digits=40
fn="4784969"
m=10 : m.BigNum=fpipow(m.BigNum, show_number_of_digits)
t=Timer

x=5
x=Sqr(x)
y=(Log((1+x)/2)*fn-log(x))/Log(BigFloat(10))
z=Frac(y)
y=10^z
s=Trim(fp2str(y.BigNum, show_number_of_digits))
dp=Instr(s, ".")
a=Left(s, dp-1)+Mid(s, dp+1)
dp=Instr(a,"e")
s=Left(a,dp-1)

last9=fib_mod(fn, m)
sn=fp2str(fn.BigNum, 310)
dp=Instr(sn, ".")
sn=Trim(Left(sn, dp-1))+Mid(sn, dp+1)
dp=Instr(sn,"e")
xp=Clng(Mid(sn, dp+1))
sn=Left(sn, xp+1)
sl=fp2str(last9.BigNum, show_number_of_digits+2)
sl=Trim(sl)
dp=Instr(sl, ".")
a=Left(sl, dp-1)+Mid(sl, dp+1)
dp=Instr(a,"e")
sl=Left(a,dp-1)
dp=Clng(Mid(a,dp+1))
sl=Left(sl,dp+1)
Print "the first";Len(s);" digits of Fibonacci ";sn;" are ";s
Print "the last ";show_number_of_digits;
Print " digits of Fibonacci ";sn;" are ";sl
t=timer-t
Print "time taken is";t;" seconds"
