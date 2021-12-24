Const NUMBER_OF_DIGITS = 128

#Include "DecFloat.bi"

Dim As DecFloat x, y

For x=1 To 20
	y=Sqr(x)
	Print x.toLong, y.toString_fix(64)
Next

Sleep
