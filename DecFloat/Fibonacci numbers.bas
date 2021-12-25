Const NUMBER_OF_DIGITS = 48384

#Include "DecFloat.bi"

Dim As DecFloat x, y, z
Dim As Double tm
Dim As Integer i, j
Dim As String s

s=String(100,"9")+"8"+String(101,"9")
Print "the fibonacci numbers, press return to start";
Sleep
x=s
y=1
tm=Timer
z=y/x
tm=timer-tm
s=fp2str_fix(z.dec_num)
j=Instr(s,".")
s=Mid(s,j+1)

j=1
For i=1 To 481
	Print Mid(s,j,101)
	j+=101
Next

Print "elapsed time for the division was ";tm;" seconds"

Sleep
