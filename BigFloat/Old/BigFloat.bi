Extern "c"
	Declare Function __builtin_clz (Byval x As Ulong) As Long
	Declare Function __builtin_sadd_overflow (Byval As Long, Byval As Long, Byref As Long) As Boolean
	Declare Function __builtin_saddll_overflow (Byval As Longint, Byval As Longint, Byref As Longint) As Boolean
	Declare Function __builtin_uadd_overflow (Byval As Ulong, Byval As Ulong, Byref As Ulong) As Boolean
	Declare Function __builtin_uaddll_overflow (Byval As Ulongint, Byval As Ulongint, Byref As Ulongint) As Boolean

	Declare Function __builtin_ssub_overflow (Byval As Long, Byval As Long, Byref As Long) As Boolean
	Declare Function __builtin_ssubll_overflow (Byval As Longint, Byval As Longint, Byref As Longint) As Boolean
	Declare Function __builtin_usub_overflow (Byval As Ulong, Byval As Ulong, Byref As Ulong) As Boolean
	Declare Function __builtin_usubll_overflow (Byval As Ulongint, Byval As Ulongint, Byref As Ulongint) As Boolean

	Declare Function __builtin_smul_overflow (Byval As Long, Byval As Long, Byref As Long) As Boolean
	Declare Function __builtin_smulll_overflow (Byval As Longint, Byval As Longint, Byref As Longint) As Boolean
	Declare Function __builtin_umul_overflow (Byval As Ulong, Byval As Ulong, Byref As Ulong) As Boolean
	Declare Function __builtin_umulll_overflow (Byval As Ulongint, Byval As Ulongint, Byref As Ulongint) As Boolean
End Extern

#Ifndef NUMBER_OF_DIGITS
	Const NUMBER_OF_DIGITS = 128
#Endif
                        
Const NUMBER_OF_BITS   =  NUMBER_OF_DIGITS*3.321928094887362

#If (NUMBER_OF_BITS Mod 32)<>0
	Const NUM_BITS = (NUMBER_OF_BITS\32+1)*32
#Else
	Const NUM_BITS = NUMBER_OF_BITS
#Endif

Const NUM_DWORDS    =   NUM_BITS\32
Const BIAS          =   1073741824 '2 ^ 30

' Error definitions

#Define DIVZ_ERR        1                    'Divide by zero
#Define EXPO_ERR        2                    'Exponent overflow error
#Define EXPU_ERR        3                    'Exponent underflow error

Type BigFloat_struct
	Declare Constructor ( )
	Declare Constructor ( Byref rhs As BigFloat_struct )
	Declare Destructor ( )
	Declare Operator Let ( Byref rhs As BigFloat_struct )
	As Short sign
	As Ulong  exponent
	As Ulong  Ptr mantissa
End Type

Constructor BigFloat_struct ( )
	If this.mantissa<>0 Then
		Print "pointer is not 0"
		End(1)
	End If
	this.mantissa=Callocate((NUM_DWORDS +1), 4)
	If this.mantissa=0 Then
		Print "unable to allocate memory"
		End(4)
	End If
	this.sign=0
	this.exponent=0
End Constructor

Constructor BigFloat_struct ( Byref rhs As BigFloat_struct )
	If this.mantissa<>0 Then
		Print "pointer is not 0"
		End(1)
	End If
	this.mantissa=Callocate((NUM_DWORDS +1), 4)
	If this.mantissa=0 Then
		Print "unable to allocate memory"
		End(4)
	End If
	this.sign=rhs.sign
	this.exponent=rhs.exponent
	For i As Long=0 To NUM_DWORDS
		this.mantissa[i]=rhs.mantissa[i]
	Next
End Constructor

Operator BigFloat_struct.let ( Byref rhs As BigFloat_struct )
	this.sign=rhs.sign
	this.exponent=rhs.exponent
	For i As Long=0 To NUM_DWORDS
		this.mantissa[i]=rhs.mantissa[i]
	Next
End Operator

Destructor BigFloat_struct ( )
	If this.mantissa=0 Then
		Print "unable to de-allocate memory"
		End(-4)
	Else
		Deallocate(this.mantissa)
	End If
	this.mantissa=0
End Destructor

Type BigFloat
	Declare Constructor ( )
	Declare Constructor ( Byval rhs As Long )
	Declare Constructor ( Byval rhs As Double )
	Declare Constructor ( Byref rhs As String )
	Declare Constructor ( Byref rhs As BigFloat )
	Declare Destructor ( )
	
	Declare Operator Let ( Byval rhs As Long )
	Declare Operator Let ( Byval rhs As Double )
	Declare Operator Let ( Byref rhs As String )
	Declare Operator Let ( Byref rhs As BigFloat )
	Declare Operator Cast ( ) As String
	Declare Operator Cast ( ) As Long
	Declare Operator Cast ( ) As Double
	
	'----------------------------------------------
	Declare Operator += (Byref rhs As BigFloat)
	Declare Operator += (Byval rhs As Long)
	Declare Operator += (Byval rhs As Double)
	Declare Operator += (Byref rhs As String)
	Declare Operator -= (Byref rhs As BigFloat)
	Declare Operator -= (Byval rhs As Long)
	Declare Operator -= (Byval rhs As Double)
	Declare Operator -= (Byref rhs As String)
	Declare Operator *= (Byref rhs As BigFloat)
	Declare Operator *= (Byval rhs As Long)
	Declare Operator *= (Byval rhs As Double)
	Declare Operator *= (Byref rhs As String)
	Declare Operator /= (Byref rhs As BigFloat)
	Declare Operator /= (Byval rhs As Long)
	Declare Operator /= (Byval rhs As Double)
	Declare Operator /= (Byval rhs As Single)
	Declare Operator /= (Byref rhs As String)

	' For Next Implicit step = +1
	Declare Operator For ( )
	Declare Operator Step( )
	Declare Operator Next( Byref end_cond As BigFloat ) As Integer
	' For Next Exlicit step
	Declare Operator For ( Byref stp As BigFloat )
	Declare Operator Step( Byref stp As BigFloat )
	Declare Operator Next( Byref end_cond As BigFloat, Byref step_var As BigFloat ) As Integer
	
	Declare Function toString( Byval places As Long=NUM_DWORDS*9.63 ) As String
	Declare Function toLong ( ) As Long
	Declare Function toDouble ( ) As Double

	BigNum As BigFloat_struct

End Type

Declare Function fpcmp(Byref x As BigFloat_struct, Byref y As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As Long
Declare Function fpadd(Byref x As BigFloat_struct, Byref y As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
Declare Function fpsub(Byref x As BigFloat_struct, Byref y As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
Declare Function fpmul(Byref x As BigFloat_struct, Byref y As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
Declare Function fpmul_si(Byref x As BigFloat_struct, Byval y As Long, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
Declare Function fpdiv_si(Byref num As BigFloat_struct, Byval den As Long, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
Declare Function fpdiv(Byref x As BigFloat_struct, Byref y As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
Declare Function fpipow(Byref x As BigFloat_struct, Byval e As Longint, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
Declare Function dbl2fp(Byval x As Double) As BigFloat_struct
Declare Function fp2dbl(Byref x As BigFloat_struct) As Double
Declare Function ui2fp(Byval m As Ulong, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
Declare Function fp2ui(Byref x As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As Ulong
Declare Function si2fp(Byval m As Long, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
Declare Function fp2si(Byref x As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As Long
Declare Function str2fp(Byref x As String) As BigFloat_struct
Declare Function fp2str(Byref zz As BigFloat_struct, Byval digits As Long=NUMBER_OF_DIGITS+8) As String
Declare Function fpfix( Byref num As BigFloat_struct ) As BigFloat_struct
Declare Function fpfrac( Byref num As BigFloat_struct ) As BigFloat_struct
Declare Function fpsqr (Byref num As bigfloat_struct, Byval dwords As Long=NUM_DWORDS) As bigfloat_struct
Declare Function mp_sin(Byref x As BigFloat_struct, Byval dwords As Ulong = NUM_DWORDS) As BigFloat_struct
Declare Function mp_cos (Byref x As BigFloat_struct, Byval dwords As Ulong = NUM_DWORDS) As BigFloat_struct
Declare Function mp_tan(Byref x As BigFloat_struct, Byval dwords As Ulong = NUM_DWORDS) As BigFloat_struct
Declare Function fpatn (Byref x As BigFloat_struct, Byval dwords As Ulong = NUM_DWORDS) As BigFloat_struct
Declare Function fpasin (Byref x As BigFloat_struct, Byval dwords As Ulong = NUM_DWORDS) As BigFloat_struct
Declare Function fpacos (Byref x As BigFloat_struct, Byval dwords As Ulong = NUM_DWORDS) As BigFloat_struct
Declare Function fpnroot(Byref x As BigFloat_struct, Byval p As Long, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
Declare Function fplog (x As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
Declare Function fpexp (Byref x As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct

Function Bigfloat.toString( Byval places As Long=NUM_DWORDS*9.63 ) As String
	Function = fp2str( this.BigNum, places )
End Function

Function Bigfloat.toLong ( ) As Long
	Dim As Double x
	x=fp2dbl(this.BigNum)
	Return Clng(x)
End Function


Function Bigfloat.toDouble ( ) As Double
	Function = fp2dbl(this.BigNum)
End Function

Constructor Bigfloat ( )
	this.BigNum=si2fp(0, NUM_DWORDS)
End Constructor

Constructor Bigfloat ( Byval rhs As Long )
	this.BigNum=si2fp( rhs, NUM_DWORDS )
End Constructor

Constructor Bigfloat ( Byref rhs As String )
	this.BigNum=str2fp( rhs )
End Constructor

Constructor Bigfloat ( Byref rhs As Bigfloat)
	this.BigNum.sign=rhs.BigNum.sign
	this.BigNum.exponent=rhs.BigNum.exponent
	For i As Long=0 To NUM_DWORDS
		this.BigNum.mantissa[i]=rhs.BigNum.mantissa[i]
	Next
End Constructor

Constructor Bigfloat ( Byval rhs As Double )
	this.BigNum=dbl2fp( rhs )
End Constructor

Operator Bigfloat.let ( Byref rhs As Bigfloat )
	this.BigNum.sign=rhs.BigNum.sign
	this.BigNum.exponent=rhs.BigNum.exponent
	For i As Long=0 To NUM_DWORDS
		this.BigNum.mantissa[i]=rhs.BigNum.mantissa[i]
	Next
End Operator

Operator Bigfloat.let ( Byval rhs As Long )
	this.BigNum=si2fp( rhs, NUM_DWORDS )
End Operator

Operator Bigfloat.let ( Byref rhs As String )
	this.BigNum=str2fp( rhs )
End Operator

Operator Bigfloat.Let ( Byval rhs As Double )
	this.BigNum=dbl2fp( rhs )
End Operator

Operator Bigfloat.cast ( ) As String
	Operator = fp2str(this.BigNum)
End Operator

Operator Bigfloat.cast ( ) As Long
	Dim As Double x
	x=fp2dbl(this.BigNum)
	Operator = Clng(x)
End Operator

Operator Bigfloat.cast ( ) As Double
	Operator = fp2dbl(this.BigNum)
End Operator

Destructor Bigfloat ( )
End Destructor

'============================================================================

Operator BigFloat.for ( )
End Operator
 
Operator BigFloat.step ( )
	this.BigNum = fpadd(this.BigNum,si2fp(1, NUM_DWORDS), NUM_DWORDS) 
End Operator 
 
Operator BigFloat.next ( Byref end_cond As BigFloat ) As Integer
	Return fpcmp(This.BigNum, end_cond.BigNum, NUM_DWORDS)<=0
End Operator
 
 
'' explicit step versions
'' 
Operator BigFloat.for ( Byref step_var As BigFloat )
End Operator
 
Operator BigFloat.step ( Byref step_var As BigFloat )
	this.BigNum = fpadd(this.BigNum, step_var.BigNum, NUM_DWORDS)	
End Operator 
 
Operator BigFloat.next ( Byref end_cond As BigFloat, Byref step_var As BigFloat ) As Integer
	If fpcmp(step_var.BigNum, si2fp(0, NUM_DWORDS), NUM_DWORDS) < 0 Then
		Return fpcmp(This.BigNum, end_cond.BigNum, NUM_DWORDS) >= 0
	Else
		Return fpcmp(This.BigNum, end_cond.BigNum, NUM_DWORDS) <= 0
	End If
End Operator

'============================================================================

Operator + ( Byref lhs As Bigfloat, Byval rhs As Bigfloat ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum= fpadd(lhs.BigNum, rhs.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator + ( Byref lhs As Bigfloat, Byval rhs As Double ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=dbl2fp( rhs )
	result.BigNum= fpadd(lhs.BigNum, result.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator + ( Byval lhs As Double, Byref rhs As Bigfloat ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=dbl2fp( lhs )
	result.BigNum= fpadd( result.BigNum, rhs.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator + ( Byref lhs As Bigfloat, Byval rhs As Long ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=si2fp( rhs )
	result.BigNum= fpadd(lhs.BigNum, result.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator + ( Byval lhs As Long, Byref rhs As Bigfloat ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=si2fp( lhs )
	result.BigNum= fpadd( result.BigNum, rhs.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator + (Byref lhs As Bigfloat, Byref rhs As String) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=str2fp(rhs)
	result.BigNum= fpadd(lhs.BigNum, result.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator - ( Byref rhs As Bigfloat ) As Bigfloat
	Dim As Bigfloat result = rhs
	result.BigNum.sign = result.BigNum.sign Xor &h8000
	Operator = result
End Operator

Operator - ( Byref lhs As Bigfloat, Byval rhs As Bigfloat ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum= fpsub(lhs.BigNum, rhs.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator - ( Byref lhs As Bigfloat, Byval rhs As Double ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=dbl2fp( rhs )
	result.BigNum= fpsub(lhs.BigNum, result.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator - ( Byval lhs As Double, Byref rhs As Bigfloat ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=dbl2fp( lhs )
	result.BigNum= fpsub( result.BigNum, rhs.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator - ( Byref lhs As Bigfloat, Byval rhs As Long ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=si2fp(rhs, NUM_DWORDS)
	result.BigNum= fpsub(lhs.BigNum, result.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator - ( Byval lhs As Long, Byref rhs As Bigfloat  ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=si2fp(lhs, NUM_DWORDS)
	result.BigNum= fpsub(result.BigNum, rhs.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator - (Byref lhs As Bigfloat, Byref rhs As String) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=str2fp(rhs)
	result.BigNum= fpsub(lhs.BigNum, result.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator * ( Byref lhs As Bigfloat, Byval rhs As Bigfloat ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum= fpmul(lhs.BigNum, rhs.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator * ( Byref lhs As Bigfloat, Byval rhs As Double ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=dbl2fp( rhs )
	result.BigNum= fpmul(lhs.BigNum, result.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator * ( Byval lhs As Double, Byref rhs As Bigfloat ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=dbl2fp( lhs )
	result.BigNum= fpmul( result.BigNum, rhs.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator * ( Byref lhs As Bigfloat, Byval rhs As Long ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=si2fp(rhs, NUM_DWORDS)
	result.BigNum= fpmul(result.BigNum, lhs.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator * ( Byval lhs As Long, Byref rhs As Bigfloat  ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=si2fp(lhs, NUM_DWORDS)
	result.BigNum= fpmul(result.BigNum, rhs.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator * (Byref lhs As Bigfloat, Byref rhs As String) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=str2fp(rhs)
	result.BigNum= fpmul(lhs.BigNum, result.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator / ( Byref lhs As Bigfloat, Byval rhs As Bigfloat ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum= fpdiv(lhs.BigNum, rhs.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator / ( Byref lhs As Bigfloat, Byval rhs As Double ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=dbl2fp( rhs )
	result.BigNum= fpdiv(lhs.BigNum, result.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator / ( Byval lhs As Double, Byref rhs As Bigfloat ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=dbl2fp( lhs )
	result.BigNum= fpdiv( result.BigNum, rhs.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator / ( Byref lhs As Bigfloat, Byval rhs As Long ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=si2fp(rhs, NUM_DWORDS)
	result.BigNum= fpdiv(lhs.BigNum, result.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator / ( Byval lhs As Long, Byref rhs As Bigfloat  ) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=si2fp(lhs, NUM_DWORDS)
	result.BigNum= fpdiv(result.BigNum, rhs.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator / (Byref lhs As Bigfloat, Byref rhs As String) As Bigfloat
	Dim As Bigfloat result
	result.BigNum=str2fp(rhs)
	result.BigNum= fpdiv(lhs.BigNum, result.BigNum, NUM_DWORDS)
	Operator = result
End Operator

Operator ^ ( Byref lhs As Bigfloat, Byval rhs As Longint ) As Bigfloat
	Dim As Bigfloat lhs2
	lhs2.BigNum=fpipow(lhs.BigNum, rhs, NUM_DWORDS)
	Operator = lhs2
End Operator

Operator ^ ( Byref lhs As Bigfloat, Byref rhs As Bigfloat ) As Bigfloat
	Dim As Bigfloat lhs2
	lhs2.BigNum=fplog(lhs.BigNum, NUM_DWORDS)
	lhs2.BigNum=fpmul(lhs2.BigNum, rhs.BigNum, NUM_DWORDS)
	lhs2.BigNum=fpexp(lhs2.BigNum, NUM_DWORDS)
	Operator = lhs2
End Operator

Operator BigFloat.+= (Byref rhs As BigFloat)
	this.BigNum = fpadd(this.BigNum, rhs.BigNum, NUM_DWORDS)
End Operator

Operator BigFloat.+= (Byval rhs As Long)
	Dim As BigFloat result
	result.BigNum=si2fp(rhs, NUM_DWORDS)
	this.BigNum = fpadd(this.BigNum, result.BigNum, NUM_DWORDS)
End Operator

Operator BigFloat.+= (Byval rhs As Double)
	Dim As BigFloat result
	result.BigNum=dbl2fp( rhs )
	this.BigNum = fpadd(this.BigNum, result.BigNum, NUM_DWORDS)
End Operator

Operator BigFloat.+= (Byref rhs As String)
	Dim As BigFloat result
	result.BigNum=str2fp(rhs)
	this.BigNum = fpadd(this.BigNum, result.BigNum, NUM_DWORDS)
End Operator

Operator BigFloat.-= (Byref rhs As BigFloat)
	this.BigNum = fpsub(this.BigNum, rhs.BigNum, NUM_DWORDS)
End Operator

Operator BigFloat.-= (Byval rhs As Long)
	Dim As BigFloat result
	result.BigNum=si2fp(rhs, NUM_DWORDS)
	this.BigNum = fpsub(this.BigNum, result.BigNum, NUM_DWORDS)
End Operator

Operator BigFloat.-= (Byval rhs As Double)
	Dim As BigFloat result
	result.BigNum=dbl2fp( rhs )
	this.BigNum = fpsub(this.BigNum, result.BigNum, NUM_DWORDS)
End Operator

Operator BigFloat.-= (Byref rhs As String)
	Dim As BigFloat result
	result.BigNum=str2fp(rhs)
	this.BigNum = fpsub(this.BigNum, result.BigNum, NUM_DWORDS)
End Operator

Operator BigFloat.*= (Byref rhs As BigFloat)
	this.BigNum = fpmul(this.BigNum, rhs.BigNum, NUM_DWORDS)
End Operator

Operator BigFloat.*= (Byval rhs As Long)
	Dim As BigFloat result
	result.BigNum=si2fp(rhs, NUM_DWORDS)
	this.BigNum = fpmul(this.BigNum, result.BigNum, NUM_DWORDS)
End Operator

Operator BigFloat.*= (Byval rhs As Double)
	Dim As BigFloat result
	result.BigNum=dbl2fp( rhs )
	this.BigNum = fpmul(this.BigNum, result.BigNum, NUM_DWORDS)
End Operator

Operator BigFloat.*= (Byref rhs As String)
	Dim As BigFloat result
	result.BigNum=str2fp(rhs)
	this.BigNum = fpmul(this.BigNum, result.BigNum, NUM_DWORDS)
End Operator

Operator BigFloat./= (Byref rhs As BigFloat)
	this.BigNum = fpdiv(this.BigNum, rhs.BigNum, NUM_DWORDS)
End Operator

Operator BigFloat./= (Byval rhs As Long)
	Dim As BigFloat result
	result.BigNum=si2fp(rhs, NUM_DWORDS)
	this.BigNum = fpdiv(this.BigNum, result.BigNum, NUM_DWORDS)
End Operator

Operator BigFloat./= (Byval rhs As Double)
	Dim As BigFloat result
	result.BigNum=dbl2fp( rhs )
	this.BigNum = fpdiv(this.BigNum, result.BigNum, NUM_DWORDS)
End Operator

Operator BigFloat./= (Byref rhs As String)
	Dim As BigFloat result
	result.BigNum=str2fp(rhs)
	this.BigNum = fpdiv(this.BigNum, result.BigNum, NUM_DWORDS)
End Operator

Operator = ( Byref lhs As BigFloat, Byref rhs As BigFloat ) As Long
	Operator = fpcmp(lhs.BigNum, rhs.BigNum, NUM_DWORDS)=0
End Operator

Operator < ( Byref lhs As BigFloat, Byref rhs As BigFloat ) As Long
	Operator = fpcmp(lhs.BigNum, rhs.BigNum, NUM_DWORDS)<0
End Operator

Operator > ( Byref lhs As BigFloat, Byref rhs As BigFloat ) As Long
	Operator = fpcmp(lhs.BigNum, rhs.BigNum, NUM_DWORDS)>0
End Operator

Operator <= ( Byref lhs As BigFloat, Byref rhs As BigFloat ) As Long
	Operator = fpcmp(lhs.BigNum, rhs.BigNum, NUM_DWORDS)<=0
End Operator

Operator >= ( Byref lhs As BigFloat, Byref rhs As BigFloat ) As Long
	Operator = fpcmp(lhs.BigNum, rhs.BigNum, NUM_DWORDS)>=0
End Operator

Operator <> ( Byref lhs As BigFloat, Byref rhs As BigFloat ) As Long
	Operator = fpcmp(lhs.BigNum, rhs.BigNum, NUM_DWORDS)<>0
End Operator

Operator Abs(Byref rhs As BigFloat) As BigFloat
	Dim As BigFloat result
	result.BigNum=rhs.BigNum
	result.BigNum.sign = 0
	Operator = result
End Operator

Operator  Fix (Byref x As BigFloat) As BigFloat
	Dim As BigFloat result
	result.BigNum =fpfix(x.BigNum)
	Operator = result
End Operator

Operator Frac(Byref x As BigFloat) As BigFloat
	Dim As BigFloat result
	result.BigNum=fpfrac(x.BigNum)
	Operator = result
End Operator

Operator Sqr(Byref x As BigFloat) As BigFloat
	Dim As BigFloat result
	result.BigNum=fpsqr(x.BigNum)
	Operator = result
End Operator

Operator Sin(Byref x As BigFloat) As BigFloat
	Dim As BigFloat result
	result.BigNum=mp_sin(x.BigNum)
	Operator = result
End Operator

Operator Cos(Byref x As BigFloat) As BigFloat
	Dim As BigFloat result
	result.BigNum=mp_cos(x.BigNum)
	Operator = result
End Operator

Operator Tan(Byref x As BigFloat) As BigFloat
	Dim As BigFloat result
	result.BigNum=mp_tan(x.BigNum)
	Operator = result
End Operator

Operator Atn(Byref x As BigFloat) As BigFloat
	Dim As BigFloat result
	result.BigNum=fpatn(x.BigNum)
	Operator = result
End Operator

Operator Asin(Byref x As BigFloat) As BigFloat
	Dim As BigFloat result
	result.BigNum=fpasin(x.BigNum)
	Operator = result
End Operator

Operator Acos(Byref x As BigFloat) As BigFloat
	Dim As BigFloat result
	result.BigNum=fpatn(x.BigNum)
	Operator = result
End Operator

Operator Exp(Byref x As BigFloat) As BigFloat
	Dim As BigFloat result
	result.BigNum=fpexp(x.BigNum)
	Operator = result
End Operator

Operator Log(Byref x As BigFloat) As BigFloat
	Dim As BigFloat result
	result.BigNum=fplog(x.BigNum)
	Operator = result
End Operator

Dim Shared As BigFloat_struct Log102
Log102.sign=0
Log102.exponent=(-2)+BIAS+1
If NUM_DWORDS>=0 Then Log102.mantissa[0]=&h13441350
If NUM_DWORDS>=1 Then Log102.mantissa[1]=&h9F79FEF3
If NUM_DWORDS>=2 Then Log102.mantissa[2]=&h11F12B35
If NUM_DWORDS>=3 Then Log102.mantissa[3]=&h816F922F

Private Function log2_32(Byval value As Ulong) As Long
'https://stackoverflow.com/questions/11376288/fast-computing-of-log2-for-64-bit-integers
	Static tab32(0 To 31) As Const Long = {0, 9, 1, 10, 13, 21, 2, 29, 11, 14, 16, 18, 22, 25, 3, 30, 8, 12, 20, 28, 15, 17, 24, 7, 19, 27, 23, 6, 26, 5, 4, 31}
	value Or= Culng(value Shr 1)
	value Or= Culng(value Shr 2)
	value Or= Culng(value Shr 4)
	value Or= Culng(value Shr 8)
	value Or= Culng(value Shr 16)
	Return tab32(Culng(Culng(value * &h07C4ACDD) Shr 27))
End Function

Private Function log2_64(Byval value As Ulongint) As Long
'https://stackoverflow.com/questions/11376288/fast-computing-of-log2-for-64-bit-integers
	Static tab64(0 To 63) As Const Long ={63, 0, 58, 1, 59, 47, 53, 2, 60, 39, 48, 27, 54, 33, 42, 3, 61, 51, 37, 40, 49, 18, 28, 20, 55, 30, 34, 11, 43, 14, 22, 4, 62, 57, 46, 52, 38, 26, 32, 41, 50, 36, 17, 19, 29, 10, 13, 21, 56, 45, 25, 31, 35, 16, 9, 12, 44, 24, 15, 8, 23, 7, 6, 5}
	value Or= Culngint(value Shr 1)
	value Or= Culngint(value Shr 2)
	value Or= Culngint(value Shr 4)
	value Or= Culngint(value Shr 8)
	value Or= Culngint(value Shr 16)
	value Or= Culngint(value Shr 32)
	Return tab64((Culngint((value - (value Shr 1)) * &h07EDD5E59A4E28C2) Shr 58))
End Function

Function ipower (Byval x As Ulongint, Byval e As Ulongint) As Ulongint
	'take x to an integer power
	Dim As Ulongint z, y, n
	y = x
	n = e
	z = 1
	While n > 0
		While (n And 1) = 0
			n = n \ 2
			y = y * y
		Wend
			n = n - 1
			z = y * z
	Wend
	Return z
End Function

Function shl32(Byval n As Ulong, Byval k As Ubyte, Byref c As Ulong) As Ulong
	If k>0 And k<32 Then
		Dim As Ulong carry=n
		Dim As Ubyte k32=32-k
		Asm
			mov cl, [k32]
			Shr dword Ptr [carry], cl
			mov cl, [k]
			Shl dword Ptr [n], cl
		End Asm
		c=carry
	End If
	Return n
End Function

Function shr32(Byval n As Ulong, Byval k As Ubyte, Byref c As Ulong) As Ulong
	If k>0 And k<32 Then
		Dim As Ulong carry=n
		Dim As Ubyte k32=32-k
		Asm
			mov cl, [k32]
			Shl dword Ptr [carry], cl
			mov cl, [k]
			Shr dword Ptr [n], cl
		End Asm
		c=carry
	End If
	Return n
End Function

Sub shiftl(Byref fac1 As BigFloat_struct, Byval k As Long, Byval dwords As Long=NUM_DWORDS)
	If k>0 And k<32 Then
		Dim As Integer i
		Dim As Ulong n, carry, c=0
		Dim As Long k32=32-k
		For i=dwords To 0 Step -1
			n=fac1.mantissa[i]
			carry=n
			carry=Cast(Ulong, (carry Shr k32))
			n=Cast(Ulong, (n Shl k))
			fac1.mantissa[i]=n+c
			c=carry
		Next
	Elseif k=32 Then
		Dim As Long i
		For i=0 To dwords-1
			fac1.mantissa[i]=fac1.mantissa[i+1]
		Next
		fac1.mantissa[dwords]=0
	End If
End Sub

Sub shiftr(Byref fac1 As BigFloat_struct, Byval k As Long, Byval dwords As Long=NUM_DWORDS)
	If k>0 And k<32 Then
		Dim As Long i
		Dim As Ulong n, carry, c=0
		Dim As Long k32=32-k
		For i=0 To dwords
			n=fac1.mantissa[i]
			carry=n
			carry=Cast(Ulong, (carry Shl k32))
			n=Cast(Ulong, (n Shr k))   
			fac1.mantissa[i]=c+n
			c=carry
		Next
	Elseif k=32 Then
		Dim As Integer i
		For i=dwords To 1 Step -1
			fac1.mantissa[i]=fac1.mantissa[i-1]
		Next
		fac1.mantissa[0]=0
	End If
End Sub

Function fpcmp(Byref x As BigFloat_struct, Byref y As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As Long
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
		Dim As Long i
		Dim As Longint c
		If x.sign < y.sign Then
			Return -1
		End If
		If x.sign > y.sign Then
			Return 1
		End If
		If x.exponent<y.exponent Then
			If x.sign=0 Then
				Return -1
			Else
				Return 1
			End If
		End If
		If x.exponent>y.exponent Then
			If x.sign=0 Then
				Return 1
			Else
				Return -1
			End If
		End If

		For i=0 To dwords
			c=Clngint(x.mantissa[i])-clngint(y.mantissa[i])
			If c<>0 Then Exit For
		Next
		If c=0 Then Return 0
		If c<0 Then
			If x.sign=0 Then
				Return -1
			Else
				Return 1
			End If
		End If
		If c>0 Then
			If x.sign=0 Then
				Return 1
			Else
				Return -1
			End If
		End If
End Function

Function NORM_FAC1(Byref fac1 As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As Integer
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
	' normalize the number in fac1
	' all routines exit through this one.

	'see if the mantissa is all zeros.
	'if so, set the exponent and sign equal to 0.
	Dim As Integer i,er=0,f=0
	For i=0 To dwords
		If fac1.mantissa[i]>0 Then f=1
	Next
	If f=0 Then
		fac1.exponent=0
		fac1.sign=0
		Exit Function
	End If

	While fac1.mantissa[0]=0
		shiftl(fac1, 32, dwords)
		fac1.exponent-=32
		If fac1.exponent=0 Then
			NORM_FAC1=EXPU_ERR
			Exit Function
		End If
	Wend
	i=__builtin_clz(fac1.mantissa[0])-3
	If i>0 Then
		shiftl(fac1, i, dwords)
		fac1.exponent-=i
	End If
	'if the highmost Bit in fac1_man is nonzero,
	'shift the mantissa right 1 Bit and
	'increment the exponent      
	If fac1.mantissa[0]>(&h1ffffffful)Then
		While fac1.mantissa[0]>&h1ffffffful
		shiftr(fac1, 1, dwords)
		fac1.exponent+=1
		Wend
	Elseif fac1.mantissa[0]<&h10000000ul Then
		/' the following will probably never be executed '/
		'now shift fac1_man 1 to the left until a
		'nonzero Bit appears in the next-to-highest
		'Bit of fac1_man.  decrement exponent for
		'each shift.

		While fac1.mantissa[0]<&h10000000ul
			shiftl(fac1, 1, dwords)
			fac1.exponent-=1
		Wend
	End If

	'check for overflow/underflow
	If fac1.exponent<0 Then
	?"NORM_FAC1 fac1.exponent<0"
	NORM_FAC1=EXPO_ERR
	End If
End Function

Function si2fp(Byval m As Long, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
	Dim As BigFloat_struct fac1
	Dim As Long n=Abs(m)

	If m=0 Then
		Return fac1
	End If

	fac1.mantissa[0]=n
	NORM_FAC1(fac1, NUM_DWORDS)
	fac1.exponent=log2_32(n)+BIAS+1   
	If m<0 Then
		fac1.sign=&H8000
	Else
		fac1.sign=0
	End If
	Return fac1
End Function

Function ui2fp(Byval m As Ulong, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
	Dim As BigFloat_struct fac1

	If m=0 Then
		Return fac1
	End If

	fac1.mantissa[0]=m
	NORM_FAC1(fac1, NUM_DWORDS)
	fac1.exponent=log2_32(m)+BIAS+1   
	Return fac1
End Function

Sub fpadd_aux(Byref fac1 As BigFloat_struct, Byref fac2 As BigFloat_struct, Byval dwords As Long=NUM_DWORDS)
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
	Dim As Long c
	Dim As Integer i
	Dim As Longint v

	c=0
	For i=dwords To 1 Step -1
		v=Clngint(fac2.mantissa[i])+Clngint(fac1.mantissa[i])+c
		If v>(&h100000000ull-1) Then
			v=v-&h100000000ull
			c=1
		Else
			c=0
		End If
		fac1.mantissa[i]=v
	Next
	v=Clngint(fac1.mantissa[0])+Clngint(fac2.mantissa[0])+c
	fac1.mantissa[0]=v
	NORM_FAC1(fac1, dwords)

End Sub

Sub fpsub_aux(Byref fac1 As BigFloat_struct, Byref fac2 As BigFloat_struct, Byval dwords As Long=NUM_DWORDS)
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
	Dim As Long c
	Dim As Integer i
	Dim As Longint v
	c=0
	For i=dwords To 1 Step -1
		v=Clngint(fac1.mantissa[i])-clngint(fac2.mantissa[i])-c
		If v<0 Then
			v=v+&h100000000ull
			c=1
		Else
			c=0
		End If
		fac1.mantissa[i]=v
	Next
	v=Clngint(fac1.mantissa[0])-clngint(fac2.mantissa[0])-c
	fac1.mantissa[0]=v
	NORM_FAC1(fac1, dwords)
End Sub

Function fpadd(Byref x As BigFloat_struct, Byref y As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS

	Dim As BigFloat_struct fac1,fac2
	Dim As Long i, t, c, xsign, ysign

	xsign=x.sign:x.sign=0
	ysign=y.sign:y.sign=0
	c=fpcmp(x, y, dwords)

	x.sign=xsign
	y.sign=ysign
	If c<0 Then
		fac1=y
		fac2=x
	Else
		fac1=x
		fac2=y
	End If
	t=fac1.exponent-fac2.exponent

	If t<(NUM_BITS+32) Then
		'The difference between the two
		'exponents indicate how many times
		'we have to multiply the mantissa
		'of FAC2 by 10 (i.e., shift it right 1 place).
		'If we have to shift more times than
		'we have dwords, the result is already in FAC1.

		If t>0 And t<(NUM_BITS+32) Then 'shift

			i=t\32
			While i>0
				shiftr(fac2, 32, dwords)
				t-=32
				i-=1
			Wend
			'While t>0

			If t>0 Then   shiftr(fac2, t, dwords)
				't-=1
				'Wend
			End If
			'See if the signs of the two numbers
			'are the same.  If so, add; if not, subtract.
			If fac1.sign=fac2.sign Then 'add
				fpadd_aux(fac1,fac2, dwords)
			Else
				fpsub_aux(fac1,fac2, dwords)
			End If
	Endif
	Return fac1
End Function

Function fpsub(Byref x As BigFloat_struct, Byref y As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
	Dim As BigFloat_struct fac1,fac2
	Dim As Long s
	fac1=x
	fac2=y
	fac2.sign=fac2.sign Xor &h8000
	fac1=fpadd(fac1,fac2, dwords)
	Return fac1
End Function

Function fpmul(Byref x As BigFloat_struct, Byref y As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
	Dim As BigFloat_struct fac1,fac2
	Dim As Integer i, j, ex, er, den, num
	Dim As Ulongint digit, carry, prod ', tmp
	Dim As Ulong  fac3(0 To 2*dwords+1)

	fac1=x
	fac2=y
	'check exponents.  if either is zero,
	'the result is zero
	If fac1.exponent=0 Or fac2.exponent=0 Then 'result is zero...clear fac1.
		fac1.sign=0
		fac1.exponent=0
		For i=0 To dwords
			fac1.mantissa[i]=0
		Next
		'NORM_FAC1(fac1)
		Return fac1
	Else

		If ex<0 Then
			er=EXPO_ERR
			Return fac1 'Exit Function
		End If

		'clear fac3 mantissa
		For i=0 To dwords
			fac3(i)=0
		Next

		den=dwords
		While fac2.mantissa[den]=0
			den-=1
		Wend
		num=dwords
		While fac1.mantissa[num]=0
			num-=1
		Wend

		If num<den Then
			Swap fac1, fac2
			'fac1=y
			'fac2=x         
			Swap den, num
		End If

		For j=den To 0 Step -1
			carry=0
			digit=fac2.mantissa[j]
			For i=num To 0 Step -1
				prod=fac3(i+j+1)+digit*fac1.mantissa[i]+carry
				/'tmp=fac1.mantissa[i]
				If __builtin_umulll_overflow (digit, tmp, prod) Then Print "mul overflow"
				tmp=fac3(i+j+1)
				If __builtin_uaddll_overflow (tmp, prod, prod) Then Print "add overflow"
				If __builtin_uaddll_overflow (carry, prod, prod) Then Print "add overflow"'/
				carry=prod   Shr 32'\&H100000000
				fac3(i+j+1)=prod '(prod mod &H100000000)
			Next

			fac3(j)=carry
		Next

		For i=0 To dwords
			fac1.mantissa[i]=fac3(i)
		Next      

	End If
	'now determine exponent of result.
	'as you do...watch for overflow.
	'ex=fac2.exponent-BIAS+fac1.exponent
	'fac1.exponent=ex

	ex=(fac2.exponent And &h7FFFFFFFul)-BIAS+1
	ex=ex+(fac1.exponent And &h7FFFFFFFul)-BIAS+1
	fac1.exponent=ex+BIAS+1

	'determine the sign of the product
	fac1.sign=fac1.sign Xor fac2.sign
	NORM_FAC1(fac1, dwords)
	Return fac1
End Function

Function fpmul_si(Byref x As BigFloat_struct, Byval y As Long, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
    Dim As BigFloat_struct fac1,fac2
    Dim As Integer count, ex, er, i, j
    Dim As Long n, v, c, num
    Dim As Longint carry, digit, prod, value
    fac1=x
    digit=Abs(y)
    If digit>&h7fffffff Then
		fac2=si2fp(y, dwords)
		fac1=fpmul(fac1, fac2, dwords)
		Return fac1
	End If
    'check exponents.  if either is zero,
    'the result is zero
    If fac1.exponent=0 Or y=0 Then 'result is zero...clear fac1.
        fac1.sign=0
        fac1.exponent=0
        For count=0 To dwords
            fac1.mantissa[count]=0
        Next
        NORM_FAC1(fac1, dwords)
        Return fac1
    Else
		If digit=1 Then
			If y<0 Then
				fac1.sign=fac1.sign Xor &h8000
			End If
			Return fac1
		End If
        'now determine exponent of result.
        'as you do...watch for overflow.

        If ex<0 Then
            er=EXPO_ERR
            Return fac1 'Exit Function
        End If
		num=dwords
		While fac1.mantissa[num]=0
			num-=1
		Wend        
		carry=0
			
		For i=num To 0 Step -1
			prod=digit*fac1.mantissa[i] +carry
			value=(prod Mod &h100000000)'+carry
			fac1.mantissa[i]=value
			carry=prod\&h100000000
		Next

		n=carry
		i=__builtin_clz(n)
		shiftr(fac1, (32-i)+3)
		fac1.exponent+=(32-i+3)
		n=shl32(n, i-3,c)
		fac1.mantissa[0]+=n
    End If

    NORM_FAC1(fac1, dwords)

    If y<0 Then
		fac1.sign=fac1.sign Xor &h8000
	End If
    Return fac1
End Function

Function fpmul_ui(Byref x As BigFloat_struct, Byval y As Ulong, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
    Dim As BigFloat_struct fac1,fac2
    Dim As Integer count, ex, er, i, j
    Dim As Ulong n, v, c, num
    Dim As Ulongint carry, digit, prod, value
    fac1=x
    digit=Abs(y)
    If digit>&hfffffffful Then
		fac2=si2fp(y, dwords)
		fac1=fpmul(fac1, fac2, dwords)
		Return fac1
	End If
    'check exponents.  if either is zero,
    'the result is zero
    If fac1.exponent=0 Or y=0 Then 'result is zero...clear fac1.
        fac1.sign=0
        fac1.exponent=0
        For count=0 To dwords
            fac1.mantissa[count]=0
        Next
        NORM_FAC1(fac1, dwords)
        Return fac1
    Else
		If digit=1 Then
			Return fac1
		End If
        'now determine exponent of result.
        'as you do...watch for overflow.

        If ex<0 Then
            er=EXPO_ERR
            Return fac1 'Exit Function
        End If
		num=dwords
		While fac1.mantissa[num]=0
			num-=1
		Wend        
		carry=0
			
		For i=num To 0 Step -1
			prod=digit*fac1.mantissa[i] +carry
			value=(prod Mod &h100000000)'+carry
			fac1.mantissa[i]=value
			carry=prod\&h100000000
		Next

		n=carry
		i=__builtin_clz(n)
		shiftr(fac1, (32-i)+3)
		fac1.exponent+=(32-i+3)
		n=shl32(n, i-3,c)
		fac1.mantissa[0]+=n
    End If

    NORM_FAC1(fac1, dwords)

    Return fac1
End Function

Function fpmul_ll(Byref x As BigFloat_struct, Byval y As Longint, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
    Dim As BigFloat_struct fac1,fac2
    Dim As Integer count, ex, er, i, j
    Dim As Long n, v, c
    Dim As Longint carry, digit, prod, value, n0, n1
    fac1=x
    digit=Abs(y)
    If digit>&h7FFFFFFFl And digit<=&h7FFFFFFFFFFFFFFFll Then
		n0=digit\&h100000000
		n1=digit Mod &h100000000
		fac2=fpmul_ui(fac1, n1, dwords)
		fac1.exponent+=32
		fac1=fpmul_ui(fac1, n0, dwords)
		fac1=fpadd(fac1, fac2, dwords)
		If y<0 Then fac1.sign=fac1.sign Xor &h8000
		Return fac1
	Elseif digit>&h7FFFFFFFFFFFFFFFll Then
		fac2=str2fp(Str(y))
		fac1=fpmul(fac1, fac2, dwords)
		Return fac1
	Else
		fac1=fpmul_si(fac1, Clng(y), dwords)
	End If

    If y<0 Then
		fac1.sign=fac1.sign Xor &h8000
	End If
    Return fac1
End Function

Function fpmul_ull(Byref x As BigFloat_struct, Byval y As Ulongint, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
    Dim As BigFloat_struct fac1,fac2
    Dim As Integer count, ex, er, i, j
    Dim As Ulong n, v, c
    Dim As Ulongint carry, digit, prod, value, n0, n1
    fac1=x
    digit=y
    If digit>&hFFFFFFFFul Andalso digit<=&hFFFFFFFFFFFFFFFFull Then
		n0=digit\&h100000000
		n1=digit Mod &h100000000
		fac2=fpmul_ui(fac1, n1, dwords)
		fac1.exponent+=32
		fac1=fpmul_ui(fac1, n0, dwords)
		fac1=fpadd(fac1, fac2, dwords)
		Return fac1
	Elseif digit>&hFFFFFFFFFFFFFFFFull Then
		fac2=str2fp(Str(y))
		fac1=fpmul(fac1, fac2, dwords)
		Return fac1
	Else
		fac1=fpmul_ui(fac1, Culng(y), dwords)
	End If

    Return fac1
End Function

Function fpdiv_si(Byref num As BigFloat_struct, Byval den As Long, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
	Dim As BigFloat_struct fac1
	Dim As Ulongint carry, remder=0
	Dim As Longint i, divisor
	Dim As Longint quotient
	Dim As Long j

	divisor=Abs(den)
	fac1=num
	If divisor=1 Then
		Return fac1
	End If
	If divisor = 0 Then
		Print "error: divisor = 0"
		Return fac1 'Exit function
	End If
	If divisor>2147483647 Then
		Print "error: divisor too large"
		Return fac1 'Exit function
	End If

	For i = 0 To dwords
		quotient = fac1.mantissa[i] + remder * &h100000000ull
		remder = quotient Mod divisor
		fac1.mantissa[i]=quotient \ divisor
	Next
	quotient = remder * &h100000000ull
	quotient=quotient \ divisor
	carry=fac1.mantissa[0]
	j=__builtin_clz(fac1.mantissa[0])-3
	If j>0 Then
		shiftl(fac1, j, dwords)
		fac1.exponent-=j
	End If

	While fac1.mantissa[0]>(&h1ffffffful)
		shiftr(fac1, 1, dwords)
		fac1.exponent+=1
	Wend
	NORM_FAC1(fac1)
	If den<0 Then
	fac1.sign=fac1.sign Xor &h8000
	End If
	Return fac1
End Function


Function fpdiv(Byref x As BigFloat_struct, Byref y As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
	Dim As BigFloat_struct fac1, fac2, one
	Dim As Integer i, er, is_power_of_two
	Dim As Short sign
	fac1=x
	fac2=y

	one.exponent=(BIAS+1)
	one.mantissa[0]=&h10000000ul
	one=ui2fp(1)
	sign=fac2.sign
	fac2.sign=0
	If fpcmp(fac2, one)=0 Then
		Return fac1
	Else
		fac2.sign=sign
	End If
	If fac2.exponent=0 Then ' if fac2 = 0, return
		' a divide-by-zero error and
		' bail out.
		fac1.mantissa[i]=&H1FFFFFFFul
		For i=1 To dwords
			fac1.mantissa[i]=&HFFFFFFFFul
		Next
		fac1.exponent=&HFFFFF+BIAS+1
		er=DIVZ_ERR
		Return fac1
	Elseif fac1.exponent=0 Then 'fact1=0, just return
		er=0
		Return fac1
	Else
		'check to see if fac2 is a power of ten
		is_power_of_two=0
		If fac2.mantissa[0]=&H10000000ul Then
			is_power_of_two=1
			For i=1 To dwords
				If fac2.mantissa[i]<>0 Then
					is_power_of_two=0
					Exit For
				End If
			Next
		End If
		'if fac2 is a power of ten then all we need to do is to adjust the sign and exponent and we are finished
		If is_power_of_two=1 Then
			fac1.sign=fac1.sign Xor fac2.sign
			fac1.exponent=fac1.exponent-fac2.exponent+BIAS+1
			Return fac1
		End If

		#Define min(a,b) Iif(((a)<(b)),(a),(b))

		#Macro realw(w, j)
			((w(j - 1)*b + w(j))*b + w(j + 1))*b +Iif(Ubound(w)>=j+2,w(j+2),0)
		#Endmacro

		#Macro subtract(w, q, d, ka, kb)
			For j=ka To kb
				w(j) = w(j) - q*d(j - ka + 2)
			Next
		#Endmacro

		#Macro normalize(w, ka, q)
			w(ka) = w(ka) + w(ka - 1)*b
			w(ka - 1) = q
		#Endmacro

		#Macro finalnorm(w, kb)
			For j=kb To 3 Step -1
				carry=Iif(w(j)<0, ((-w(j) - 1)\b) + 1, Iif(w(j) >= b, -(w(j)\b), 0))
				w(j) = w(j) + carry*b
				w(j - 1) = w(j - 1) - carry
			Next
		#Endmacro

		Dim As Double result(1 To 2*dwords+3), n(1 To 2*dwords+3), d(1 To 2*dwords+3)
		Const b=&H10000   
		Dim As Integer j, last, laststep, q, t
		Dim As Integer stp, carry
		Dim As Double xd, xn, rund
		Dim As Double w(1 To Ubound(n)+4)

		For j=0 To dwords
			n(2*j+2)=fac1.mantissa[j]\&H10000
			n(2*j+3)=fac1.mantissa[j] Mod &H10000
			d(2*j+2)=fac2.mantissa[j]\&H10000
			d(2*j+3)=fac2.mantissa[j] Mod &H10000
		Next
		n(1)=(fac1.exponent And &h7FFFFFFF)-BIAS-1
		d(1)=(fac2.exponent And &h7FFFFFFF)-BIAS-1
		'For j=Ubound(n) To Ubound(w)
		'	w(j)=0
		'Next
		t=Ubound(n)-1
		w(1)=n(1)-d(1)+1
		w(2)=0
		For j=2 To Ubound(n)
			w(j+1)=n(j)
		Next   
		xd = (d(2)*b + d(3))*b + d(4) + d(5)/b
		laststep = t + 2
		For stp=1 To laststep
			xn=RealW(w, (stp + 2))
			q=Int(xn/xd)
			last = Min(stp + t + 1, Ubound(W))
			subtract(w, q, d, (stp + 2), last)
			normalize(w, (stp + 2), q)
		Next
		FinalNorm(w, (laststep + 1))
		laststep = Iif(w(2) = 0, laststep, laststep - 1)
		rund = w(laststep + 1)/b
		w(laststep) = w(laststep) + Iif(rund >= 0.5, 1, 0)
		If w(2)=0 Then
			For j=1 To t+1
				result(j)=w(j+1)
			Next
		Else
			For j=1 To t+1
				result(j)=w(j)
			Next
		End If
		result(1) = Iif(w(2) = 0, w(1) - 1, w(1))

		For j=0 To dwords
			fac1.mantissa[j]=result(2*j+2)*&H10000+result(2*j+3)
		Next
		NORM_FAC1(fac1, dwords)
		fac1.exponent=(result(1)+BIAS)
	End If
	fac1.sign=fac1.sign Xor fac2.sign
	Return fac1
End Function

Function fpipow(Byref x As BigFloat_struct, Byval e As Longint, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
	'take x to an Long power
	Dim As BigFloat_struct y=x
	Dim As BigFloat_struct z, one
	Dim As Longint n, c=0
	Dim As Integer i

	n = Abs(e)
	one.exponent=(BIAS+1)
	one.mantissa[0]=&h10000000ul
	z=one
	While n>0
		While (n And 1)=0
			n\=2
			y=fpmul(y, y, dwords)
			c+=1
		Wend
		n-=1
		z=fpmul(y, z, dwords)
		c+=1
	Wend

	If e<0 Then
		z=fpdiv(one, z, dwords)
	End If

	Return z
End Function

Function dbl2fp(Byval x As Double) As BigFloat_struct
	Dim As BigFloat_struct bf
	Dim As Ulongint n, e
	Dim As Long sign
	If x<0 Then sign=&h8000
	x=Abs(x)
	n=Peek ( Ulongint, @x )
	e=n
	n=Cast(Ulongint, (n Shl 12))
	n=Cast(Ulongint, (n Shr 4))
	n+=&h1000000000000000ull
	bf.mantissa[0]=Cast(Ulongint, (n Shr 32))
	n=e
	n=Cast(Ulongint, (n Shl 40))
	n=Cast(Ulongint, (n Shr 32))
	bf.mantissa[1]=n
	n=n+&h1000000000000000ull
	e=Cast(Ulongint, (e Shr 52))
	e=e-&h3FFul
	bf.exponent=e+BIAS+1
	bf.sign=sign
	Return bf
End Function

Function fp2dbl(Byref x As BigFloat_struct) As Double
	Dim As Double dbl
	Dim As Ulongint n, e
	Dim As Ushort ex
	Dim As Long sign
	If x.sign<>0 Then
		sign=-1
	Else
		sign=1
	End If
	If x.exponent>0 Then
		ex=(x.exponent And &h7FFFFFFF)-BIAS-1
	Else
		ex=0
	End If
	n=x.mantissa[0]
	n-=&h10000000ul
	n=Cast(Ulongint, (n Shl 24))
	e=&h3FF+ex
	e=e*&h10000000000000ull
	n=n+e
	e=x.mantissa[1]\&h100
	n=n+e
	Poke Ulongint,@dbl,n
	Return dbl*sign
End Function

Function factorial(Byval n As Ulong) As BigFloat_struct
	Dim As BigFloat_struct one, f, indx
	one.mantissa[0]=&h10000000ul
	one.exponent=0+BIAS+1
	f=one
	indx=one
	For i As Long=1 To n
		f=fpmul(f, indx)
		indx=fpadd(one, indx)
	Next
	Return f
End Function

Function factorial_ui(Byval n As Ulong) As BigFloat_struct
	Dim As BigFloat_struct one, f
	one.mantissa[0]=&h10000000ul
	one.exponent=0+BIAS+1
	f=one
	For i As Long=1 To n
		f=fpmul_si(f, i)
	Next
	Return f
End Function

Function factorial3(Byval n As Ulong) As BigFloat_struct
	Dim As BigFloat_struct one, f
	one.mantissa[0]=&h10000000ul
	one.exponent=0+BIAS+1
	f=one
	For i As Ulongint=1 To n
		f=fpmul_ull(f, i)
	Next
	Return f
End Function

Function factorial_inv(Byval n As Ulong) As BigFloat_struct
	Dim As BigFloat_struct f
	f.mantissa[0]=&h10000000ul
	f.exponent=0+BIAS+1
	For i As Long=1 To n
		f=fpdiv_si(f, i)
	Next
	Return f
End Function

Function fp2ui(Byref x As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As Ulong
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
	Dim As BigFloat_struct fac1
	Dim As Double n
	fac1.exponent=0
	fac1.mantissa[0]=x.mantissa[0]
	n=fp2dbl(x)
	Return Fix(n)
End Function

Function fp2si(Byref x As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As Long
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
	Dim As BigFloat_struct fac1
	Dim As Long sign
	Dim As Double n

	fac1.exponent=0
	fac1.mantissa[0]=x.mantissa[0]
	n=fp2dbl(x)
	Return Fix(n)
End Function

Function str2fp_aux(Byval value As String, Byref s As Long, Byref ex As Long ) As String
	Dim As Integer j,d,e,ep,es,i,f,fp,fln
	Dim As String c,f1,f2,f3, ts
	Dim As Ulong ulng

	j=1
	s=1
	d=0
	e=0
	ep=0
	ex=0
	es=1
	i=0
	f=0
	fp=0
	f1=""
	f2=""
	f3=""
	value=Ucase(value)
	fln=Len(value)

	While j<=fln
		c=Mid(value,j,1)
		If ep=1 Then
			If c=" " Then
				j=j+1
				Continue While
			Endif
			If c="-" Then
				es=-es
				c=""
			Endif
			If c="+" Then
				j=j+1
				Continue While
			Endif
			If (c="0") And (f3="") Then
				j=j+1
				Continue While
			Endif
			If (c>"/") And (c<":") Then 'c is digit between 0 and 9
				f3=f3+c
				ex=10*ex+(Asc(c)-48)
				j=j+1
				Continue While
			Endif
		Endif

		If c=" " Then
			j=j+1
			Continue While
		Endif
		If c="-" Then
			s=-s
			j=j+1
			Continue While
		Endif
		If c="+" Then
			j=j+1
			Continue While
		Endif
		If c="." Then
			If d=1 Then
				j=j+1
				Continue While
			Endif
			d=1
		Endif
		If (c>"/") And (c<":") Then 'c is digit between 0 and 9
			If ((c="0") And (i=0)) Then
				If d=0 Then
					j=j+1
					Continue While
				Endif
				If (d=1) And (f=0) Then
					e=e-1
					j=j+1
					Continue While
				Endif
			Endif
			If d=0 Then
				f1=f1+c
				i=i+1
			Else
				If (c>"0") Then
					fp=1
				Endif
				f2=f2+c
				f=f+1
			Endif
		Endif
		If c="E" Or c="D" Then
		ep=1
		Endif
		j=j+1
	Wend
	If fp=0 Then
		f=0
		f2=""
	Endif

	If s=-1 Then s=&h8000 Else s=0

	f1=f1+f2
	f1=Mid(f1,1,1)+Right(f1,Len(f1)-1)
	fln=Len(f1)
	If fp=0 Andalso ex=0 Then
		ex=es*fln-1
	Else
		ex=es*ex-1+i+e
	End If
	If fln>((NUM_BITS+16)*0.3010299956639811) Then
		f1=Mid(f1,1,((NUM_BITS+16)*0.3010299956639812)-1)
	Endif

	Return f1
End Function

Function str2fp(Byref x As String) As BigFloat_struct
	Dim As Long strlen, i, n, ex, sign, ten
	Dim As String s2, strin=x
	Dim As BigFloat_struct y, z, pw

	strin=str2fp_aux(strin, sign, ex)
	strlen=Len(strin)

	n=8
	ten=Valuint("1"+String(n,"0"))
	If strlen>n Then
		s2=Left(strin, n)
		strin=Mid(strin, n+1)
	Else
		z=ui2fp(Valuint(strin))
		pw=ui2fp(10)
		pw=fpipow(pw, strlen-ex-1)
		z=fpdiv(z, pw)
		Return z
	End If
	z=ui2fp(Valuint(s2))

	While Len(strin)>=n
		s2=Left(strin, n)
		strin=Mid(strin, n+1)
		y=ui2fp(Valuint(s2))
		z=fpmul_ui(z, ten)
		z=fpadd(z,y)
	Wend
	i=Len(strin)
	If i>0 Then
		ten=Valuint("1"+String(i,"0"))
		y=ui2fp(Valuint(strin))
		z=fpmul_ui(z, ten)
		z=fpadd(z,y)
	End If
	pw=ui2fp(10)
	pw=fpipow(pw,strlen-ex-1)
	z=fpdiv(z, pw)
	z.sign=sign
	Return z
End Function

Function fp2str(Byref zz As BigFloat_struct, Byval digits As Long=NUMBER_OF_DIGITS+8) As String
	If digits>(NUMBER_OF_DIGITS+8) Or digits<0 Then digits=NUMBER_OF_DIGITS+8
	Dim As Long ex, i, m, n, powten, sign, tn
	Dim As BigFloat_struct y, ten, tenp, z, ep, one, nine, zero
	Dim As String s, s2
	If zz.exponent=0 Then
		Return "0"
	End If
	digits+=2
	n=8
	one=ui2fp(1)
	nine=ui2fp(9)
	ten=ui2fp(10)
	tenp=ui2fp(Clng("1"+String(n,"0")))
	tn=Clng("1"+String(n,"0"))
	z=zz
	sign=zz.sign
	z.sign=0
	If z.exponent<>0 Then
		ex=(z.exponent And &h7FFFFFFFul)-BIAS-1
	Else
		ex=0
	End If
	m=NUM_BITS-ex
	ep.exponent=-(m+16)+BIAS+1
	ep.mantissa[0]=&h10000000ul
	z=fpadd(z,ep)
	If fpcmp(z,nine)>0 Then
		While fpcmp(z,nine)>0
			z=fpdiv_si(z, 10)
			powten+=1
		Wend
	Elseif fpcmp(z,one)<0 Then
		While fpcmp(z,one)<0
			z=fpmul(z, ten)
			powten-=1
		Wend
	End If

	m=fp2ui(z)
	z=fpfrac(z)
	z=fpmul(z, tenp)
	s=Trim(Str(m))+"."
	For i=1 To digits
		If fpcmp(z, one)<0 Then
			While fpcmp(z, one)<0
				If (Len(s))>=digits Then Exit While
				z=fpmul(z, tenp)
				s=s+String(n,"0")
			Wend
		Else
			If (Len(s))>=digits Then Exit For
			y=fpfix(z)
			m=fp2ui(y)
			z=fpfrac(z)
			z=fpmul(z, tenp)
			s2=Trim(Str(m))
			s2=String(n-len(s2),"0")+s2
			s=s+s2
		End If	
	Next
	s=Ltrim(s, "0")
	If Len(s)>(digits-1) Then
		s=Left(s, digits-1)
	End If
	i=Instr(Trim(s),".")
	
	If i>2 Then
		s=Left(s,i-1)+Mid(s, i+1)
		s=Left(s,1)+"."+Mid(s, 2)
		powten+=1
	Elseif i=1 Then
		s=Mid(s,2)
		s=Left(s,1)+"."+Mid(s,2)
		powten-=1
	End If
	s2=Trim(Str(Abs(powten)))
	If (5-len(s2))>0 Then s2=String(5-len(s2),"0")+s2
	If powten<0 Then
		s2="-"+s2
	Else
		s2="+"+s2
	End If
	s=s+"e"+s2
	If sign<>0 Then
		s="-"+s
	Else
		s=" "+s
	End If
	Return s
End Function

'integer part of num
Function fpfix( Byref num As BigFloat_struct ) As BigFloat_struct
	Dim As BigFloat_struct ip
	Dim As Long ex, ex2, j, k, c

	ex=(num.exponent And &h7FFFFFFFul)-BIAS
	If ex<1 Then
		Return ip
	End If
	If ex>=(NUM_BITS) Then
		Return num
	End If
	ex2=ex\32
	k=ex2
	j=ex Mod 32
	While ex2>0
		ex2-=1
		ip.mantissa[ex2]=num.mantissa[ex2]
	Wend

	ip.mantissa[k]=num.mantissa[k]
	ip.mantissa[k]=shr32(ip.mantissa[k], (32-j)-3, c)
	ip.mantissa[k]=shl32(ip.mantissa[k], (32-j)-3, c)

	ip.exponent=ex+BIAS
	ip.sign=num.sign
	NORM_FAC1(ip)
	Return ip
End Function

Function fpfix2( Byref num As BigFloat_struct ) As BigFloat_struct
	Dim As BigFloat_struct ip
	Dim As Long ex, ex2, ext, j, k, c

	ex=(num.exponent And &h7FFFFFFFul)-BIAS
	If ex<1 Then
		Return ip
	End If
	If ex>=(NUM_BITS) Then
		Return num
	End If
	ext=ex
	ex+=1
	ex2=ex\32
	k=ex2
	j=ex Mod 32
	While ex2>0
		ex2-=1
		ip.mantissa[ex2]=num.mantissa[ex2]
	Wend

	ip.mantissa[k]=num.mantissa[k]
	ip.mantissa[k]=shr32(ip.mantissa[k], (32-j)-3, c)
	ip.mantissa[k]=shl32(ip.mantissa[k], (32-j)-3, c)
	
	ex=ext
	ip.exponent=ex+BIAS
	ip.sign=num.sign
	NORM_FAC1(ip)
	Return ip
End Function

'fractional part of num
Function fpfrac( Byref num As BigFloat_struct ) As BigFloat_struct
	Dim As BigFloat_struct ip, fp
	ip=fpfix(num)
	fp=fpsub(num, ip)
	Return fp
End Function

'returns 1 if integer part is odd
Function fpfix_is_odd( Byref num As bigfloat_struct ) As Long
	Dim As Long ex, j, k, c
	Dim As Ulong m
	
	ex=(num.exponent And &h7FFFFFFF)-BIAS
	If ex<1 Then
		Return 0
	End If
	If ex>=(NUM_BITS) Then
		Print "error in function fpfix_is_odd"
		Return -1
	End If
	k=ex\32
	j=ex Mod 32
	m=num.mantissa[k]
	m=shr32(m, (32-j)-3, c)
	Return m And 1
End Function

Function pi_brent_salamin (Byval digits As Ulong = NUM_DWORDS*9.63) As bigfloat_struct
    If digits > NUM_DWORDS*9.63 Then digits = NUM_DWORDS*9.63
	
    Dim As Long limit
    Dim As bigfloat_struct c0, c1, c2, c05
    Dim As bigfloat_struct a, b, sum
    Dim As bigfloat_struct ak, bk, ck
    Dim As bigfloat_struct ab, asq
    Dim As bigfloat_struct pow2, tmp, eps
    
	eps.exponent=-(digits*3.3)+BIAS+1
	eps.mantissa[0]=&h10000000ul	
    limit = (-digits) + BIAS + 1

    c0=si2fp(0, digits): ak = c0: bk = c0: ab = c0: asq = c0
    c1=si2fp(1, digits): a = c1: ck = c1: pow2 = c1
    c2=si2fp(2, digits): b = c2
    c05=ui2fp(1) : c05=fpdiv_si(c05, 2)
    sum = c05

    b=fpsqr(b, digits)
    b=fpdiv(c1, b, digits)

    While fpcmp(ck, eps) > 0
        ak=fpadd(a, b)
        ak=fpmul(c05, ak)
        ab=fpmul(a, b)
        bk=fpsqr(ab)
        asq=fpmul(ak, ak)
        ck=fpsub(asq, ab)
        pow2=fpmul_si(pow2, 2)
        tmp=fpmul(pow2, ck)
        sum=fpsub(sum, tmp)
        a = ak: b = bk
    Wend

    tmp=fpdiv(asq, sum)
    tmp=fpmul_si(tmp, 2)
    Return tmp
End Function

Dim Shared As BigFloat pi_bf, pi2_bf, pi_bf_half, pi_bf_quarter
pi_bf.BigNum = pi_brent_salamin()
pi2_bf.BigNum = fpadd(pi_bf.BigNum, pi_bf.BigNum)
pi_bf_half.BigNum = fpdiv_si( pi_bf.BigNum, 2)
pi_bf_quarter.BigNum = fpdiv_si( pi_bf.BigNum, 4)

Dim Shared As BigFloat tan_half_num(15), tan_half_den(14)
	tan_half_num(0).BigNum=si2fp(992)
	tan_half_num(1).BigNum=str2fp("161388480")
	tan_half_num(2).BigNum=str2fp("7686610525440")
	tan_half_num(3).BigNum=str2fp("167256984742848000")
	tan_half_num(4).BigNum=str2fp("2000393537524462080000")
	tan_half_num(5).BigNum=str2fp("14467646220791919547392000")
	tan_half_num(6).BigNum=str2fp("66677300813465118447390720000")
	tan_half_num(7).BigNum=str2fp("201117789910786072985458237440000")
	tan_half_num(8).BigNum=str2fp("400342706504764747636935691468800000")
	tan_half_num(9).BigNum=str2fp("521967288977995909272835160309760000000")
	tan_half_num(10).BigNum=str2fp("435052278687602761865918494187323392000000")
	tan_half_num(11).BigNum=str2fp("221463964316902607512694240578598338560000000")
	tan_half_num(12).BigNum=str2fp("63663507608965602906315837691661069058048000000")
	tan_half_num(13).BigNum=str2fp("8994510946805140308046160658488525397688320000000")
	tan_half_num(14).BigNum=str2fp("470550277118574335327341015729793791741132800000000")
	tan_half_num(15).BigNum=str2fp("3827142253897737927329040261268989506161213440000000")


	tan_half_den(0).BigNum=si2fp(491040)
	tan_half_den(1).BigNum=str2fp("39540177600")
	tan_half_den(2).BigNum=str2fp("1232419887578880")
	tan_half_den(3).BigNum=str2fp("19569067214913216000")
	tan_half_den(4).BigNum=str2fp("180435497084706479616000")
	tan_half_den(5).BigNum=str2fp("1036847979156754234229760000")
	tan_half_den(6).BigNum=str2fp("3857758118493338995884748800000")
	tan_half_den(7).BigNum=str2fp("9452536125806945430316537159680000")
	tan_half_den(8).BigNum=str2fp("15257505370126034271052104685977600000")
	tan_half_den(9).BigNum=str2fp("15972199042726674823748755905478656000000")
	tan_half_den(10).BigNum=str2fp("10480804895655884717678945541785518080000000")
	tan_half_den(11).BigNum=str2fp("4060172679143214471066061077274302873600000000")
	tan_half_den(12).BigNum=str2fp("837419984702547545921539095790310985302016000000")
	tan_half_den(13).BigNum=str2fp("75810877980214754024960496978688999780515840000000")
	tan_half_den(14).BigNum=str2fp("1913571126948868963664520130634494753080606720000000")

Function fptan_half(Byref x As bigfloat_struct, Byval dwords As Ulong = NUM_DWORDS) As bigfloat_struct
	If dwords > NUM_DWORDS Then dwords = NUM_DWORDS
	Dim As bigfloat_struct accum, x_, xx, thn, thd
	Dim As Long i, sign

	xx=fpmul(x, x, dwords)

	'the folowing is a rational polynomial for tan(x/2) derived from the continued fraction
	'                         x^2
	'tan(x/2) = 2 - -------------------------
	'                           x^2
	'              6 - ----------------------
	'                             x^2
	'                 10 - ------------------
	'                               x^2
	'                     14 - --------------
	'                                 x^2
	'                          18 - ---------
	'                                   x^2
	'                              22 - -----
	'                                   26 - ...

	' and so on
	'
	'the nice quality of this expansion is that you can calculate sin, cos and tan very easily
	'
	'if y = tan(x/2) then
	'
	'          2*x*y
	'sin(x) = -------
	'         y^2+x^2
	'
	'          y^2-x^2
	'cos(x) = ---------
	'          y^2+x^2
	'
	'          2*x*y
	'tan(x) = -------
	'         y^2-x^2

	sign=-1
	thn=tan_half_num(0).BigNum
	For i=1 To 15
		thn=fpmul(xx, thn, dwords)
		If sign=1 Then
			thn=fpadd(thn, tan_half_num(i).BigNum, dwords)
		Else
			thn=fpsub(thn, tan_half_num(i).BigNum, dwords)
		End If
		sign=-sign
	Next

	thd=fpsub(xx, tan_half_den(0).BigNum, dwords)
	sign=1
	For i=1 To 14
		thd=fpmul(xx, thd, dwords)
		If sign=1 Then
			thd=fpadd(thd, tan_half_den(i).BigNum, dwords)
		Else
			thd=fpsub(thd, tan_half_den(i).BigNum, dwords)
		End If
		sign=-sign
	Next
	accum=fpdiv(thn,thd, dwords) 'tan(x/2)
	Return accum
End Function

Function mp_sin(Byref x As BigFloat_struct, Byval dwords As Ulong = NUM_DWORDS) As BigFloat_struct
	If dwords > NUM_DWORDS Then dwords = NUM_DWORDS
	Dim As BigFloat_struct ab, accum, factor, x_2, xx, p, tmp, tmp2
	Dim As BigFloat_struct thn, thd
	Dim As Long sign(0 To 3), c, limit, i

	x_2=x

	ab=x
	ab.sign=0
	If fpcmp(ab, pi2_bf.BigNum, dwords)=1 Then
		'======== centralize ==============
		'floor/ceil to centralize

		tmp=fpdiv(x_2, pi2_bf.BigNum, dwords)
		tmp2=tmp
		tmp=fpfix(tmp)     'int part
		tmp=fpsub(tmp2, tmp, dwords) 'frac part
		tmp=fpmul(tmp, pi2_bf.BigNum, dwords)
		x_2=tmp
	End If
	'? fp2str(x_2, 16)
	'==================================
	limit=9.63*dwords
	'limit = digits of precision, here's a polynomial fit to get an exstimate for limit
	'works well for precision from 60 to 10000 digits
	limit=1+Int(-0.45344993886092585968+0.022333002852398072433*limit+5.0461814408333079844e-7*limit*limit-4.2338453039804235772e-11*limit*limit*limit)
	If limit<0 Then limit=0
	factor=si2fp(5, dwords)

	For i=1 To limit
		factor=fpmul_si(factor, 5, dwords)
	Next
	'factor=factor^limit

	x_2=fpdiv(x_2,factor, dwords)  'x_2=x_2/5^limit
	accum=fptan_half(x_2, dwords)
	xx=fpmul(x_2, x_2, dwords)

	'now convert to sin(x)
	tmp=fpmul_si(x_2, 2, dwords)
	tmp=fpmul(tmp, accum, dwords)
	tmp2=fpmul(accum, accum, dwords)
	tmp2=fpadd(tmp2, xx, dwords)
	accum=fpdiv(tmp, tmp2, dwords)
	
	'multiply the result by 5^limit

	For i=1 To limit+1
		tmp=fpmul(accum, accum, dwords)
		tmp2=fpmul(accum, tmp, dwords)
		'sin(5*x) = 5 * sin(x) - 20 * sin(x)^3 + 16 * sin(x)^5
		accum=fpmul_si(accum, 5, dwords)
		accum=fpsub(accum, fpmul_si(tmp2, 20, dwords), dwords)
		tmp=fpmul(tmp2, tmp, dwords)
		accum=fpadd(accum, fpmul_si(tmp, 16, dwords), dwords)
	Next i

	Return accum
End Function

Function mp_cos (Byref x As BigFloat_struct, Byval dwords As Ulong = NUM_DWORDS) As BigFloat_struct
	If dwords > NUM_DWORDS Then dwords = NUM_DWORDS
	Dim As BigFloat_struct ab, accum, factor, x_2, xx, p, tmp, tmp2
	Dim As BigFloat_struct thn, thd
	Dim As Long sign(0 To 3), c, limit, i

	x_2=fpadd(pi_bf_half.BigNum, x, dwords)

	ab=x_2 :ab.sign=0
	If fpcmp(ab, pi2_bf.BigNum, dwords)=1 Then
		'======== centralize ==============
		'floor/ceil to centralize

		tmp=fpdiv(x_2, pi2_bf.BigNum, dwords)
		tmp2=tmp
		tmp=fpfix(tmp)     'int part
		tmp=fpsub(tmp2, tmp, dwords) 'frac part
		tmp=fpmul(tmp, pi2_bf.BigNum, dwords)
		x_2=tmp
	End If
	'==================================
	limit=9.63*dwords
	'limit = digits of precision, here's a polynomial fit to get an exstimate for limit
	'works well for precision from 60 to 10000 digits
	limit=1+Int(-0.45344993886092585968+0.022333002852398072433*limit+5.0461814408333079844e-7*limit*limit-4.2338453039804235772e-11*limit*limit*limit)
	If limit<0 Then limit=0
	factor=si2fp(5, dwords)

	For i=1 To limit
		factor=fpmul_si(factor, 5, dwords)
	Next
	'factor=factor^limit

	x_2=fpdiv(x_2,factor, dwords)  'x_2=x_2/5^limit
	accum=fptan_half(x_2, dwords)
	xx=fpmul(x_2, x_2, dwords)

	'now convert to sin(x)
	tmp=fpmul_si(x_2, 2, dwords)
	tmp=fpmul(tmp, accum, dwords)
	tmp2=fpmul(accum, accum, dwords)
	tmp2=fpadd(tmp2, xx, dwords)
	accum=fpdiv(tmp, tmp2, dwords)
	
	'multiply the result by 5^limit

	For i=1 To limit+1
		tmp=fpmul(accum, accum, dwords)
		tmp2=fpmul(accum, tmp, dwords)
		'sin(5*x) = 5 * sin(x) - 20 * sin(x)^3 + 16 * sin(x)^5
		accum=fpmul_si(accum, 5, dwords)
		accum=fpsub(accum, fpmul_si(tmp2, 20, dwords), dwords)
		tmp=fpmul(tmp2, tmp, dwords)
		accum=fpadd(accum, fpmul_si(tmp, 16, dwords), dwords)
	Next i

	Return accum
End Function

Function mp_tan(Byref x As BigFloat_struct, Byval dwords As Ulong = NUM_DWORDS) As BigFloat_struct
	If dwords > NUM_DWORDS Then dwords = NUM_DWORDS
	Dim As BigFloat_struct ab, accum, factor, x_2, xx, p, pi, pi2, circ, tmp, tmp2
	Dim As BigFloat_struct thn, thd
	Dim As Long c, limit, i, sign
	pi=pi_bf.BigNum
	pi2=pi_bf_half.BigNum 'pdiv_si(pi_bf.BigNum, 2, dwords)

	x_2=x	

	'calculate the sign of tan(x)
	'if integer part of x/(pi/2) is odd then the sign is negative
	'unless x is negative then the sign is positive
	sign=fpfix_is_odd(fpdiv(x,pi2, dwords))

	If sign=1 Then
		sign=&h8000
	End If
	circ=pi2_bf.BigNum 'fpmul_si(pi, 2, dwords)

	ab=x :ab.sign=0

	If fpcmp(ab, circ, dwords)=1 Then
		'======== centralize ==============
		'floor/ceil to centralize

		pi=pi2_bf.BigNum 'fpadd(pi, pi, dwords) 'got 2*pi
		tmp=fpdiv(x_2, pi, dwords)
		tmp2=tmp
		tmp=fpfix(tmp)     'int part
		tmp=fpsub(tmp2, tmp, dwords) 'frac part
		tmp=fpmul(tmp, pi, dwords)
		x_2=tmp
	End If
	'==================================
	limit=dwords*9.63
	'lm = dwords of precision, here's a polynomial fit to get an exstimate for limit
	'works well for precision from 60 to 10000 dwords
	limit=1+Int(-0.45344993886092585968+0.022333002852398072433*limit+5.0461814408333079844e-7*limit*limit-4.2338453039804235772e-11*limit*limit*limit)
	If limit<0 Then limit=0
	factor=si2fp(5, dwords)

	For i=1 To limit
		factor=fpmul_si(factor, 5, dwords)
	Next
	'factor=factor^limit

	x_2=fpdiv(x_2,factor, dwords)  'x_2=x_2/5^limit
	accum=fptan_half(x_2, dwords)
	xx=fpmul(x_2, x_2, dwords)

	'now convert to sin(x)
	tmp=fpmul_si(x_2, 2, dwords)
	tmp=fpmul(tmp, accum, dwords)
	tmp2=fpmul(accum, accum, dwords)
	tmp2=fpadd(tmp2, xx, dwords)
	accum=fpdiv(tmp, tmp2, dwords)

	'multiply the result by 5^limit

	For i=1 To limit+1
		tmp=fpmul(accum, accum, dwords)
		tmp2=fpmul(accum, tmp, dwords)
		'sin(5*x) = 5 * sin(x) - 20 * sin(x)^3 + 16 * sin(x)^5
		accum=fpmul_si(accum, 5, dwords)
		accum=fpsub(accum, fpmul_si(tmp2, 20, dwords), dwords)
		tmp=fpmul(tmp2, tmp, dwords)
		accum=fpadd(accum, fpmul_si(tmp, 16, dwords), dwords)
	Next i

	tmp=fpmul(accum, accum, dwords)
	tmp=fpsub(si2fp(1, dwords), tmp, dwords)
	tmp=fpsqr(tmp, dwords)
	tmp=fpdiv(accum, tmp, dwords)
	tmp.sign=sign Xor x.sign
	Return tmp
End Function

Function fpatn (Byref x As BigFloat_struct, Byval dwords As Ulong = NUM_DWORDS) As BigFloat_struct
	If dwords > NUM_DWORDS Then dwords = NUM_DWORDS
    Dim As Long sign(3): sign(3) = 1
    Dim As Long z, c = 1
    Dim As BigFloat_struct XX, Term, Accum, strC, x_2, mt, mt2, p
    Dim As BigFloat_struct Bignum, one, Bignum2, factor

    Bignum2 = x
    Bignum2.sign = 0
    one = si2fp(1, dwords)
    If fpcmp(Bignum2, one, dwords) = 0 Then
        Bignum = pi_bf_quarter.BigNum
        Bignum.sign = x.sign
        Return Bignum
    End If
    Bignum2.sign = x.sign
    Dim As Long limit = 16
    factor = si2fp(2 Shl (limit - 1), dwords)
    For z = 1 To limit
        Bignum = fpmul(Bignum2, Bignum2, dwords)
        Bignum = fpadd(Bignum, one, dwords)
        Bignum = fpsqr(Bignum, dwords)
        Bignum = fpadd(Bignum, one, dwords)
        Bignum = fpdiv(Bignum2, Bignum, dwords)
        Bignum2 = Bignum
    Next z

    mt = Bignum
    x_2 = Bignum
    p = Bignum
    XX = fpmul(x_2, x_2, dwords)
    Do
        c = c + 2
        mt2 = mt
        p = fpmul(p, XX, dwords)
        Term = fpdiv_si(p, c, dwords)
        If sign(c And 3) Then
            Accum = fpsub(mt, Term, dwords)
        Else
            Accum = fpadd(mt, Term, dwords)
        End If
        Swap mt, Accum
    Loop Until fpcmp(mt, mt2, dwords) = 0
    Return fpmul(factor, mt, dwords)
End Function

Function fpasin (Byref x As BigFloat_struct, Byval dwords As Ulong = NUM_DWORDS) As BigFloat_struct
	If dwords > NUM_DWORDS Then dwords = NUM_DWORDS
    Dim As Double num
    Dim As BigFloat_struct one, T, B, term1, minusone
    ' ARCSIN = ATN(x / SQR(-x * x + 1))
    '============= ARCSIN GUARD =========
	T = x
    one =si2fp(1, dwords)
    minusone = si2fp(-1, dwords)
    If fpcmp(T, one)>0 Then
		Return one
	End If
    If fpcmp(T, minusone)<0 Then
		Return minusone
    End If
    B = fpmul(x, x, dwords) 'x*x
    'for 1 and -1
    If fpcmp(B, one, dwords) = 0 Then
        Dim As BigFloat_struct two, Atn1
        two = si2fp(2, dwords)
        'atn1 = fpatn(one, dwords)
        If fpcmp(x, minusone, dwords) = 0 Then
            two = pi_bf_half.BigNum 'fpmul(two, atn1, dwords)
            two.sign =&h8000
            Return two 'fpmul(two, minusone, dwords)
        Else
            Return pi_bf_half.BigNum 'fpmul(two, atn1, dwords)
        End If
    End If
    B = fpsub(one, B, dwords) '1-x*x
    B = fpsqr(B, dwords) 'sqr(1-x*x)
    term1 = fpdiv(T, B, dwords)
    Return fpatn(term1, dwords)
End Function

Function fpacos (Byref x As BigFloat_struct, Byval dwords As Ulong = NUM_DWORDS) As BigFloat_struct
	If dwords > NUM_DWORDS Then dwords = NUM_DWORDS
    Dim As BigFloat_struct one, minusone, two, Atn1, tail, T, B, term1, atnterm1, zero, ep

	ep.exponent=-(NUM_BITS-20)+BIAS+1
	ep.mantissa[0]=&h10000000ul
    'ARCCOS = ATN(-x / SQR(-x * x + 1)) + 2 * ATN(1)
    '========================
    one = si2fp(1, dwords)
    minusone = si2fp(-1, dwords)
    T=fpsub(x,one)
    T.sign=0
    If fpcmp(T, ep)<=0 Then
		Return zero
	End If
    If fpcmp(x, one)>=0 Then
		Return zero
	End If
    If fpcmp(x, minusone)<0 Then
		Return pi_bf.BigNum
    End If
    two = si2fp(2, dwords)
    Atn1 = pi_bf_quarter.BigNum
    tail = pi_bf_half.BigNum '2*atn(1)
    T = x : T.sign = T.sign Xor &h8000
    B = fpmul(x, x, dwords) 'x*x
    If fpcmp(B, one, dwords) = 0 Then
        'for 1 and -1
        If fpcmp(x, minusone, dwords) = 0 Then
            Return pi_bf.BigNum
        Else
            Return si2fp(0, dwords)
        End If
    End If
    B = fpsub(one, B, dwords) '1-x*x
    B = fpsqr(B, dwords) 'sqr(1-x*x)
    term1 = fpdiv(T, B, dwords)
    atnterm1 = fpatn(term1, dwords)
    Return fpadd(atnterm1, tail, dwords)
End Function

Function fplogTaylor(x As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
    'taylor series
    '====================Log Guard==================
    Dim As BigFloat_struct g,zero
    Dim As Integer i
    g=x : g.sign=0

    If fpcmp(g, x, dwords)<>0 Then  Return zero 'Exit Function
    If fpcmp(x, zero, dwords)=0 Then  Return zero 'Exit Function
    '=============================================
    Dim As Integer invflag
    Dim As  BigFloat_struct Inv,XX,Term,Accum,strC,_x,tmp,tmp2
    Dim As BigFloat_struct T,B,one,Q,two

	one.exponent=(BIAS+1)
	one.mantissa[0]=&h10000000
	two.exponent=(1+BIAS+1)
	two.mantissa[0]=&h10000000

    _x=x
    If fpcmp(x,one, dwords)<0 Then
        invflag=1
        _x=fpdiv(one, _x, dwords)
    End If
    T=fpsub(_x, one, dwords)
    B=fpadd(_x, one, dwords)
    accum=fpdiv(T, B, dwords)
    Q=fpdiv(T, B, dwords)
    tmp=Q
    XX=fpmul(Q, Q, dwords)
    Dim As Integer c=1
    Do 
        c=c+2
        tmp2=tmp
        Q=fpmul(Q, XX, dwords)
        term=fpdiv_si(Q,c, dwords)
        Accum=fpadd(tmp,Term, dwords)
        Swap tmp,Accum
    Loop Until fpcmp(tmp,tmp2, dwords) = 0
    accum=fpmul_si(accum,2, dwords)
    If invflag Then
		accum.sign=accum.sign Xor &h8000
    End If
    Return accum
End Function

Function fplog (x As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
    '====================Log Guard==================
    Dim As BigFloat_struct g, one, zero
    Dim As Integer i, factor

	one.exponent=(BIAS+1)
	one.mantissa[0]=&h10000000
    g=x : g.sign=0
    If fpcmp(g, x, dwords)<>0 Then  Return zero 'Exit Function
    If fpcmp(x, zero, dwords)=0 Then  Return zero 'Exit Function
    If fpcmp(x, one, dwords)=0 Then  Return zero 'Exit Function
    '=============================================
    Dim As BigFloat_struct approx,ans,logx
    approx=x
    factor=8192 '4096
    approx=fpnroot(approx, factor, dwords)
    logx=fplogTaylor(approx, dwords)
    ans=fpmul_si(logx, factor, dwords)
    Return ans
End Function

Function fpexp (Byref x As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
    'taylor series
    Dim As  BigFloat_struct fac, x2, temp, accum, p, term, one
    Dim As Integer i, c, sign
	sign=x.sign
	one.exponent=(BIAS+1)
	one.mantissa[0]=&h10000000
	fac=one
	x2=x
	x2.sign=0
	If fpcmp(x2, temp, dwords)=0 Then Return fac
	c=1
    temp=fpdiv_si(x2, 8192, dwords) 'fpdiv_si(x, 67108864) '
    x2=temp
    p=x2
    accum=fpadd(fac, x2, dwords) '1 + x

    Do
        c+=1
        temp=accum
        fac=fpdiv_si(fac, c, dwords)
        p=fpmul(p, x2, dwords)
        term=fpmul(p,fac, dwords)
        Accum=fpadd(temp,Term, dwords)
    Loop Until fpcmp(accum,temp, dwords) <= 0
    For i=1 To 13
		accum=fpmul(accum, accum, dwords)
	Next
	If sign<>0 Then
		accum=fpdiv(one, accum, dwords)
	End If
    Return accum
End Function

Function fpsqr(Byref x As BigFloat_struct, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
	Dim As BigFloat_struct ry, tmp, tmp2
	Dim As Double t, t1, t2
	Dim As Long i, ex, l, prec, p=2
	
	l=Log((NUMBER_OF_DIGITS+9.63)*0.0625)*1.5
	ex=(x.exponent And &h7FFFFFFF)-BIAS-1
	t=x.mantissa[0]+x.mantissa[1]/&h100000000
	t=t/&h10000000
	t1=Log(t)/p
	t2=0.6931471805599453*ex/p
	t1=Exp(t1)
	ry=dbl2fp(t2)
	ry=fpexp(ry,1)
	tmp=dbl2fp(t1)
	ry=fpmul(ry, tmp,1)
	prec=3

	tmp=fpdiv(x, ry, prec)
	tmp2=fpadd(ry, tmp, prec)
	ry=fpdiv_si(tmp2, p, prec)	
	For i=1 To l
		prec=2*prec-1
		tmp=fpdiv(x, ry, prec)
		tmp2=fpadd(ry, tmp, prec)
		ry=fpdiv_si(tmp2, p, prec)
	Next
	Return ry
End Function

Function fpnroot(Byref x As BigFloat_struct, Byval p As Long, Byval dwords As Long=NUM_DWORDS) As BigFloat_struct
	If dwords>NUM_DWORDS Then dwords=NUM_DWORDS
	Dim As BigFloat_struct ry, tmp, tmp2
	Dim As Double t, t1, t2
	Dim As Long i, ex, l, prec
	
	l=Log((NUMBER_OF_DIGITS+9.63)*0.0625)*1.5
	ex=(x.exponent And &h7FFFFFFF)-BIAS-1
	t=x.mantissa[0]+x.mantissa[1]/&h100000000
	t=t/&h10000000
	t1=Log(t)/p
	t2=0.6931471805599453*ex/p
	t1=Exp(t1)
	ry=dbl2fp(t2)
	ry=fpexp(ry,1)
	tmp=dbl2fp(t1)
	ry=fpmul(ry, tmp,1)
	prec=3

	tmp=fpipow(ry,p-1, prec)
	tmp=fpdiv(x, tmp, prec)
	tmp2=fpmul_si(ry, p-1, prec)
	tmp2=fpadd(tmp2, tmp, prec)
	ry=fpdiv_si(tmp2, p, prec)	
	For i=1 To l
		prec=2*prec-1
		tmp=fpipow(ry,p-1, prec)
		tmp=fpdiv(x, tmp, prec)
		tmp2=fpmul_si(ry, p-1, prec)
		tmp2=fpadd(tmp2, tmp, prec)
		ry=fpdiv_si(tmp2, p, prec)
	Next
	Return ry
End Function
