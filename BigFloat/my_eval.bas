Const NUMBER_OF_DIGITS = 64
#Include "BigFloat.bi"

Namespace my_eval

Dim Shared As Integer i, id, l
Dim Shared As String aa, d_, e_, s_, ch_
Dim Shared As BigFloat g, x, v(20)

Declare Sub scan
Declare Sub unary
Declare Sub gamma
Declare Sub expon
Declare Sub term
Declare Sub expr
Declare Sub factor
Declare Sub factoriale

Sub eval (Byref ee As String, Byref xx As BigFloat)
	e_=ee
	If Len(e_) = 0 Then e_ = "0"
	If Instr(e_, ".") Then
		xx = e_
		Exit Sub
	End If

	i = 1: id = 0: l = Len(e_): s_ = ""
	scan
	expr
	If ch_ <> " " Then
		Print
		Print "Syntax Error"
		Print
	End If
	xx = v(0)
End Sub

Sub scan
	If i > l Then
		ch_ = " "
		Exit Sub
	End If
	ch_ = Mid(e_, i, 1)
	i = i + 1
	If ch_ = " " Then scan
End Sub

Sub unary
	If ch_ = "-" Or ch_ = "+" Then
		s_ = s_ + ch_
		scan
		term 'factor
		aa = Right(s_, 1)
		s_ = Left(s_, Len(s_) - 1)
		If aa <> "-" Then Exit Sub
		v(id - 1) = -v(id - 1)
		Exit Sub
	End If
	factor
End Sub

Sub gamma
	unary
	While ch_ = "!"
		x = v(id - 1)
		factoriale
		v(id - 1) = g
		scan
	Wend
End Sub

Sub expon
	gamma
	While ch_ = "^"
		scan
		gamma
		id = id - 1
		v(id - 1) = v(id - 1) ^ v(id)
	Wend
End Sub

Sub term
	expon
	While (ch_ = "*" Or ch_ = "/")
		s_ = s_ + ch_
		scan
		expon
		aa = Right(s_, 1)
		s_ = Left(s_, Len(s_) - 1)
		If aa = "*" Then
			id = id - 1
			v(id - 1) = v(id - 1) * v(id)
		End If
		If aa = "/" Then
			id = id - 1
			v(id - 1) = v(id - 1) / v(id)
		End If
	Wend
End Sub

Sub expr
	term
	While (ch_ = "-" Or ch_ = "+")
		s_ = s_ + ch_
		scan
		term
		aa = Right(s_, 1)
		s_ = Left(s_, Len(s_) - 1)
		If aa = "-" Then
			id = id - 1
			v(id - 1) = v(id - 1) - v(id)
		End If
		If aa = "+" Then
			id = id - 1
			v(id - 1) = v(id - 1) + v(id)
		End If
	Wend
End Sub
	

Sub factor
	If Instr("0123456789", ch_) Then
		d_ = ""
		While Instr("0123456789", ch_)
			d_ = d_ + ch_
			scan
		Wend
		v(id) = d_
		id = id + 1
		Exit Sub
	End If
	If ch_ = "-" Or ch_ = "+" Then unary
	If ch_ <> "(" Then
		ch_ = "@"
		Exit Sub
	End If
	scan
	expr
	If ch_ = ")" Then
		scan
	Else
		Print
		Print "Missing ')'"
	End If
End Sub


Sub factoriale
	g = 1
	For fi As Integer = 1 To x
		g = g * fi
	Next fi
End Sub

End Namespace

Dim As String s
Dim As bigfloat x

s=" "
Print "an empty expression terminates the program"
While s<>""
	Input "enter a math expression ", s
	my_eval.eval(s, x)
	Print x
Wend
