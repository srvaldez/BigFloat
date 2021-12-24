Const NUMBER_OF_DIGITS = 64
#Include "BigFloat.bi"

'op_stack used by eval
Redim Shared op_stack ( 10 ) As Zstring*6
Dim Shared As Integer op_stack_pointer=0, op_stack_ubound=10

'value_stack used by eval
Redim Shared value_stack ( 10 ) As BigFloat
Dim Shared As Integer value_stack_pointer=0, value_stack_ubound=10

Namespace my_eval

Declare Sub eval ( Byref expr_ As String, Byref result As BigFloat )

'variables used by eval
Dim Shared As Integer expr_index, expr_length
Dim Shared As String op, number, expression, char, rpn
Dim Shared As BigFloat factorial, lhs_value, rhs_value

'================================

Sub op_stack_push ( Byval n As String )
   op_stack ( op_stack_pointer )=n
   If op_stack_pointer=op_stack_ubound Then
      op_stack_ubound+=10
      Redim Preserve op_stack ( op_stack_ubound )
   End If
   op_stack_pointer+=1
End Sub

Function op_stack_pop () As String
   If op_stack_pointer=0 Then
      Print "op_stack is empty"
      Return ""
   End If
   op_stack_pointer-=1
   Return op_stack ( op_stack_pointer )
End Function

Sub value_stack_push ( Byval n As BigFloat )
   value_stack ( value_stack_pointer )=n
   If value_stack_pointer=value_stack_ubound Then
      value_stack_ubound+=10
      Redim Preserve value_stack ( value_stack_ubound )
   End If
   value_stack_pointer+=1
End Sub

Function value_stack_pop () As BigFloat
   If value_stack_pointer=0 Then
      Print "value_stack is empty"
      Return 0
   End If
   value_stack_pointer-=1
   Return value_stack ( value_stack_pointer )
End Function

'eval

Declare Sub scan
Declare Sub unary
Declare Sub gamma
Declare Sub expon
Declare Sub term
Declare Sub expr
Declare Sub factor
Declare Sub fn_factorial

Sub eval ( Byref expr_ As String, Byref result As BigFloat )
   expression=Ucase ( expr_ )
   If Len ( expression ) = 0 Then expression = "0"
   rpn=""
   expr_index = 1: expr_length = Len ( expression )
   value_stack_pointer=0
   op_stack_pointer=0
   scan
   expr
   If char <> " " Then
      Print
      Print "Syntax Error"
      Print
   End If
   ?
   result = value_stack_pop () 'value_stack ( 0 )
End Sub

Sub scan
   If expr_index > expr_length Then
      char = " "
      Exit Sub
   End If
   char = Mid ( expression, expr_index, 1 )
   expr_index = expr_index + 1
   If char = " " Then scan
End Sub

Sub unary
   If char = "-" Or char = "+" Then
      op_stack_push ( char )
      scan
      term
      op = op_stack_pop ()
      If op <> "-" Then Exit Sub
      lhs_value=value_stack_pop ()
      value_stack_push ( - lhs_value )
      rpn += "NEG "
      Exit Sub
   End If
   factor
End Sub

Sub gamma
   unary
   While char = "!"
      lhs_value = value_stack_pop ()
      fn_factorial
      value_stack_push ( factorial )
      scan
      rpn += "! "
   Wend
End Sub

Sub expon
   gamma
   While char = "^"
      scan
      gamma
      rhs_value = value_stack_pop ()
      lhs_value = value_stack_pop ()
      value_stack_push ( lhs_value ^ rhs_value )
      rpn += "^ "
   Wend
End Sub

Sub term
   expon
   While ( char = "*" Or char = "/" )
      op_stack_push ( char )
      scan
      expon
      op = op_stack_pop ()
      If op = "*" Then
         rhs_value = value_stack_pop ()
         lhs_value = value_stack_pop ()
         value_stack_push ( lhs_value * rhs_value )
         rpn += "* "
      End If
      If op = "/" Then
         rhs_value = value_stack_pop ()
         lhs_value = value_stack_pop ()
         value_stack_push ( lhs_value / rhs_value )
         rpn += "/ "
      End If
   Wend
End Sub

Sub expr
   term
   While ( char = "-" Or char = "+" )
      op_stack_push ( char )
      scan
      term
      op = op_stack_pop ()
      If op = "-" Then
         rhs_value = value_stack_pop ()
         lhs_value = value_stack_pop ()
         value_stack_push ( lhs_value - rhs_value )
         rpn += "- "
      End If
      If op = "+" Then
         rhs_value = value_stack_pop ()
         lhs_value = value_stack_pop ()
         value_stack_push ( lhs_value + rhs_value )
         rpn += "+ "
      End If
   Wend
End Sub

Sub factor
   If Instr (".0123456789", char ) Then
      number = ""
      while char<>"." andalso Instr ("0123456789", char )
        number += char
        scan
      wend
      if char="." then
         number += char
         scan
      end if
      While Instr ("0123456789", char )
         number = number + char
         scan
      Wend
      if char="E" then
         number += char
         scan
         if char="-" or char="+" then
            number += char
            scan
         end if
         if Instr ("0123456789", char ) then
          While Instr ("0123456789", char )
            number += char
            scan
          wend
         else
         number += "0"
       end if
      end if
      value_stack_push (  number )
      rpn += number+" "
      Exit Sub
   End If

   If char = "(" Then
      scan
      expr
      If char =")" Then
         scan
      else
         Print
         Print "Missing ')'"
      End If
   End If
   'functions would be added here
   'here's a very crude example just to illustrate
    if char = "S" Then
        If mid(expression, expr_index - 1, 4) = "SIN(" Then
            expr_index = expr_index + 2 'advance pointer to just before "("
            scan
            factor
            value_stack( value_stack_pointer - 1) = Sin(value_stack( value_stack_pointer - 1))
            rpn += "SIN "
        end if
    'elseif '' check for more functions
    end if
End Sub

Sub fn_factorial
   factorial = 1
   For i As Integer = 1 To lhs_value
      factorial = factorial * i
   Next i
End Sub

End Namespace

'variables used by demo
Dim As String math_expr
Dim As BigFloat result_value

' eval demo
Print "an empty expression ends the program"
math_expr=" "
While math_expr<>""
   line Input "? ",math_expr
   my_eval.eval ( math_expr, result_value )
   Print result_value
   'Print "rpn = ";my_eval.rpn
Wend
