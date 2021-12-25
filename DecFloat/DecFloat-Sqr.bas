Const NUMBER_OF_DIGITS = 128

#Include "DecFloat.bi"
fp_print_precision=50

Dim As DecFloat x, y

For x=1 To 20
	y=Sqr(x)
	Print x.toLong, y
Next

Print

For x=1 To 20
	y=Sqr(x)
	Print x.toLong, y.toString_fix(50)
Next
Sleep
