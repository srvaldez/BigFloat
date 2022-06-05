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
	union
		As Ulong  Ptr mantissa
		As Ushort Ptr wmantissa
	end union
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

Operator sgn(Byref x As BigFloat) As long
	Dim As long result
	if x<0 then
		result=-1
	elseif x=0 then
		result=0
	elseif x>0 then
		result=1
	end if
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
		fac1.sign=fac1.sign xor y.sign
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

		For j=0 To dwords*2 step 2
			n(j+3)=fac1.wmantissa[j] '\&H10000
			n(j+2)=fac1.wmantissa[j+1] 'Mod &H10000
			d(j+3)=fac2.wmantissa[j] '\&H10000
			d(j+2)=fac2.wmantissa[j+1] 'Mod &H10000
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

		For j=0 To dwords*2 step 2
			'fac1.mantissa[j]=result(2*j+2)*&H10000+result(2*j+3)
			fac1.wmantissa[j]=result(j+3)
			fac1.wmantissa[j+1]=result(j+2)
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
	i=len(strin)
	if i>0 then
		ten=Valuint("1"+String(i,"0"))
		y=ui2fp(Valuint(strin))
		z=fpmul_ui(z, ten)
		z=fpadd(z,y)
	end if
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

Dim Shared As BigFloat pi_bf, pi2_bf, pi_bf_half, pi_bf_quarter, exp1

scope
	dim as string value
        value = "2."
        value += "718281828459045235360287471352662497757247093699959574966967"
        value += "627724076630353547594571382178525166427427466391932003059921"
        value += "817413596629043572900334295260595630738132328627943490763233"
        value += "829880753195251019011573834187930702154089149934884167509244"
        value += "761460668082264800168477411853742345442437107539077744992069"
        value += "551702761838606261331384583000752044933826560297606737113200"
        value += "709328709127443747047230696977209310141692836819025515108657"
        value += "463772111252389784425056953696770785449969967946864454905987"
        value += "931636889230098793127736178215424999229576351482208269895193"
        value += "668033182528869398496465105820939239829488793320362509443117"
        value += "301238197068416140397019837679320683282376464804295311802328"
        value += "782509819455815301756717361332069811250996181881593041690351"
        value += "598888519345807273866738589422879228499892086805825749279610"
        value += "484198444363463244968487560233624827041978623209002160990235"
        value += "304369941849146314093431738143640546253152096183690888707016"
        value += "768396424378140592714563549061303107208510383750510115747704"
        value += "171898610687396965521267154688957035035402123407849819334321"
        value += "068170121005627880235193033224745015853904730419957777093503"
        value += "660416997329725088687696640355570716226844716256079882651787"
        value += "134195124665201030592123667719432527867539855894489697096409"
        value += "754591856956380236370162112047742722836489613422516445078182"
        value += "442352948636372141740238893441247963574370263755294448337998"
        value += "016125492278509257782562092622648326277933386566481627725164"
        value += "019105900491644998289315056604725802778631864155195653244258"
        value += "698294695930801915298721172556347546396447910145904090586298"
        value += "496791287406870504895858671747985466775757320568128845920541"
        value += "334053922000113786300945560688166740016984205580403363795376"
        value += "452030402432256613527836951177883863874439662532249850654995"
        value += "886234281899707733276171783928034946501434558897071942586398"
        value += "772754710962953741521115136835062752602326484728703920764310"
        value += "059584116612054529703023647254929666938115137322753645098889"
        value += "031360205724817658511806303644281231496550704751025446501172"
        value += "721155519486685080036853228183152196003735625279449515828418"
        value += "829478761085263981395599006737648292244375287184624578036192"
        value += "981971399147564488262603903381441823262515097482798777996437"
        value += "308997038886778227138360577297882412561190717663946507063304"
        value += "527954661855096666185664709711344474016070462621568071748187"
        value += "784437143698821855967095910259686200235371858874856965220005"
        value += "031173439207321139080329363447972735595527734907178379342163"
        value += "701205005451326383544000186323991490705479778056697853358048"
        value += "966906295119432473099587655236812859041383241160722602998330"
        value += "535370876138939639177957454016137223618789365260538155841587"
        value += "186925538606164779834025435128439612946035291332594279490433"
        value += "729908573158029095863138268329147711639633709240031689458636"
        value += "060645845925126994655724839186564209752685082307544254599376"
        value += "917041977780085362730941710163434907696423722294352366125572"
        value += "508814779223151974778060569672538017180776360346245927877846"
        value += "585065605078084421152969752189087401966090665180351650179250"
        value += "461950136658543663271254963990854914420001457476081930221206"
        value += "602433009641270489439039717719518069908699860663658323227870"
        value += "937650226014929101151717763594460202324930028040186772391028"
        value += "809786660565118326004368850881715723866984224220102495055188"
        value += "169480322100251542649463981287367765892768816359831247788652"
        value += "014117411091360116499507662907794364600585194199856016264790"
        value += "761532103872755712699251827568798930276176114616254935649590"
        value += "379804583818232336861201624373656984670378585330527583333793"
        value += "990752166069238053369887956513728559388349989470741618155012"
        value += "539706464817194670834819721448889879067650379590366967249499"
        value += "254527903372963616265897603949857674139735944102374432970935"
        value += "547798262961459144293645142861715858733974679189757121195618"
        value += "738578364475844842355558105002561149239151889309946342841393"
        value += "608038309166281881150371528496705974162562823609216807515017"
        value += "772538740256425347087908913729172282861151591568372524163077"
        value += "225440633787593105982676094420326192428531701878177296023541"
        value += "306067213604600038966109364709514141718577701418060644363681"
        value += "546444005331608778314317444081194942297559931401188868331483"
        value += "280270655383300469329011574414756313999722170380461709289457"
        value += "909627166226074071874997535921275608441473782330327033016823"
        value += "719364800217328573493594756433412994302485023573221459784328"
        value += "264142168487872167336701061509424345698440187331281010794512"
        value += "722373788612605816566805371439612788873252737389039289050686"
        value += "532413806279602593038772769778379286840932536588073398845721"
        value += "874602100531148335132385004782716937621800490479559795929059"
        value += "165547050577751430817511269898518840871856402603530558373783"
        value += "242292418562564425502267215598027401261797192804713960068916"
        value += "382866527700975276706977703643926022437284184088325184877047"
        value += "263844037953016690546593746161932384036389313136432713768884"
        value += "102681121989127522305625675625470172508634976536728860596675"
        value += "274086862740791285657699631378975303466061666980421826772456"
        value += "053066077389962421834085988207186468262321508028828635974683"
        value += "965435885668550377313129658797581050121491620765676995065971"
        value += "534476347032085321560367482860837865680307306265763346977429"
        value += "563464371670939719306087696349532884683361303882943104080029"
        value += "687386911706666614680001512114344225602387447432525076938707"
        value += "777519329994213727721125884360871583483562696166198057252661"
        value += "220679754062106208064988291845439530152998209250300549825704"
        value += "339055357016865312052649561485724925738620691740369521353373"
        value += "253166634546658859728665945113644137033139367211856955395210"
        value += "845840724432383558606310680696492485123263269951460359603729"
        value += "725319836842336390463213671011619282171115028280160448805880"
        value += "238203198149309636959673583274202498824568494127386056649135"
        value += "252670604623445054922758115170931492187959271800194096886698"
        value += "683703730220047531433818109270803001720593553052070070607223"
        value += "399946399057131158709963577735902719628506114651483752620956"
        value += "534671329002599439766311454590268589897911583709341937044115"
        value += "512192011716488056694593813118384376562062784631049034629395"
        value += "002945834116482411496975832601180073169943739350696629571241"
        value += "027323913874175492307186245454322203955273529524024590380574"
        value += "450289224688628533654221381572213116328811205214648980518009"
        value += "202471939171055539011394331668151582884368760696110250517100"
        value += "739276238555338627255353883096067164466237092264680967125406"
        value += "186950214317621166814009759528149390722260111268115310838731"
        value += "761732323526360583817315103459573653822353499293582283685100"
        value += "781088463434998351840445170427018938199424341009057537625776"
        value += "757111809008816418331920196262341628816652137471732547772778"
        value += "348877436651882875215668571950637193656539038944936642176400"
        value += "312152787022236646363575550356557694888654950027085392361710"
        value += "550213114741374410613444554419210133617299628569489919336918"
        value += "472947858072915608851039678195942983318648075608367955149663"
        value += "644896559294818785178403877332624705194505041984774201418394"
        value += "773120281588684570729054405751060128525805659470304683634459"
        value += "265255213700806875200959345360731622611872817392807462309468"
        value += "536782310609792159936001994623799343421068781349734695924646"
        value += "975250624695861690917857397659519939299399556754271465491045"
        value += "686070209901260681870498417807917392407194599632306025470790"
        value += "177452751318680998228473086076653686685551646770291133682756"
        value += "310722334672611370549079536583453863719623585631261838715677"
        value += "411873852772292259474337378569553845624680101390572787101651"
        value += "296663676445187246565373040244368414081448873295784734849000"
        value += "301947788802046032466084287535184836495919508288832320652212"
        value += "810419044804724794929134228495197002260131043006241071797150"
        value += "279343326340799596053144605323048852897291765987601666781193"
        value += "793237245385720960758227717848336161358261289622611812945592"
        value += "746276713779448758675365754486140761193112595851265575973457"
        value += "301533364263076798544338576171533346232527057200530398828949"
        value += "903425956623297578248873502925916682589445689465599265845476"
        value += "269452878051650172067478541788798227680653665064191097343452"
        value += "887833862172615626958265447820567298775642632532159429441803"
        value += "994321700009054265076309558846589517170914760743713689331946"
        value += "909098190450129030709956622662030318264936573369841955577696"
        value += "378762491885286568660760056602560544571133728684020557441603"
        value += "083705231224258722343885412317948138855007568938112493538631"
        value += "863528708379984569261998179452336408742959118074745341955142"
        value += "035172618420084550917084568236820089773945584267921427347756"
        value += "087964427920270831215015640634134161716644806981548376449157"
        value += "390012121704154787259199894382536495051477137939914720521952"
        value += "907939613762110723849429061635760459623125350606853765142311"
        value += "534966568371511660422079639446662116325515772907097847315627"
        value += "827759878813649195125748332879377157145909106484164267830994"
        value += "972367442017586226940215940792448054125536043131799269673915"
        value += "754241929660731239376354213923061787675395871143610408940996"
        value += "608947141834069836299367536262154524729846421375289107988438"
        value += "130609555262272083751862983706678722443019579379378607210725"
        value += "427728907173285487437435578196651171661833088112912024520404"
        value += "868220007234403502544820283425418788465360259150644527165770"
        value += "004452109773558589762265548494162171498953238342160011406295"
        value += "071849042778925855274303522139683567901807640604213830730877"
        value += "446017084268827226117718084266433365178000217190344923426426"
        value += "629226145600433738386833555534345300426481847398921562708609"
        value += "565062934040526494324426144566592129122564889356965500915430"
        value += "642613425266847259491431423939884543248632746184284665598533"
        value += "231221046625989014171210344608427161661900125719587079321756"
        value += "969854401339762209674945418540711844643394699016269835160784"
        value += "892451405894094639526780735457970030705116368251948770118976"
        value += "400282764841416058720618418529718915401968825328930914966534"
        value += "575357142731848201638464483249903788606900807270932767312758"
        value += "196656394114896171683298045513972950668760474091542042842999"
        value += "354102582911350224169076943166857424252250902693903481485645"
        value += "130306992519959043638402842926741257342244776558417788617173"
        value += "726546208549829449894678735092958165263207225899236876845701"
        value += "782303809656788311228930580914057261086588484587310165815116"
        value += "753332767488701482916741970151255978257270740643180860142814"
        value += "902414678047232759768426963393577354293018673943971638861176"
        value += "420900406866339885684168100387238921448317607011668450388721"
        value += "236436704331409115573328018297798873659091665961240202177855"
        value += "885487617616198937079438005666336488436508914480557103976521"
        value += "469602766258359905198704230017946553679"
        
        if NUMBER_OF_DIGITS<10000 then
			value=left(value, NUMBER_OF_DIGITS+9)
		end if
		
		exp1=value
		
		value = "3."
		value += "141592653589793238462643383279502884197169399375105820974944"
		value += "592307816406286208998628034825342117067982148086513282306647"
		value += "093844609550582231725359408128481117450284102701938521105559"
		value += "644622948954930381964428810975665933446128475648233786783165"
		value += "271201909145648566923460348610454326648213393607260249141273"
		value += "724587006606315588174881520920962829254091715364367892590360"
		value += "011330530548820466521384146951941511609433057270365759591953"
		value += "092186117381932611793105118548074462379962749567351885752724"
		value += "891227938183011949129833673362440656643086021394946395224737"
		value += "190702179860943702770539217176293176752384674818467669405132"
		value += "000568127145263560827785771342757789609173637178721468440901"
		value += "224953430146549585371050792279689258923542019956112129021960"
		value += "864034418159813629774771309960518707211349999998372978049951"
		value += "059731732816096318595024459455346908302642522308253344685035"
		value += "261931188171010003137838752886587533208381420617177669147303"
		value += "598253490428755468731159562863882353787593751957781857780532"
		value += "171226806613001927876611195909216420198938095257201065485863"
		value += "278865936153381827968230301952035301852968995773622599413891"
		value += "249721775283479131515574857242454150695950829533116861727855"
		value += "889075098381754637464939319255060400927701671139009848824012"
		value += "858361603563707660104710181942955596198946767837449448255379"
		value += "774726847104047534646208046684259069491293313677028989152104"
		value += "752162056966024058038150193511253382430035587640247496473263"
		value += "914199272604269922796782354781636009341721641219924586315030"
		value += "286182974555706749838505494588586926995690927210797509302955"
		value += "321165344987202755960236480665499119881834797753566369807426"
		value += "542527862551818417574672890977772793800081647060016145249192"
		value += "173217214772350141441973568548161361157352552133475741849468"
		value += "438523323907394143334547762416862518983569485562099219222184"
		value += "272550254256887671790494601653466804988627232791786085784383"
		value += "827967976681454100953883786360950680064225125205117392984896"
		value += "084128488626945604241965285022210661186306744278622039194945"
		value += "047123713786960956364371917287467764657573962413890865832645"
		value += "995813390478027590099465764078951269468398352595709825822620"
		value += "522489407726719478268482601476990902640136394437455305068203"
		value += "496252451749399651431429809190659250937221696461515709858387"
		value += "410597885959772975498930161753928468138268683868942774155991"
		value += "855925245953959431049972524680845987273644695848653836736222"
		value += "626099124608051243884390451244136549762780797715691435997700"
		value += "129616089441694868555848406353422072225828488648158456028506"
		value += "016842739452267467678895252138522549954666727823986456596116"
		value += "354886230577456498035593634568174324112515076069479451096596"
		value += "094025228879710893145669136867228748940560101503308617928680"
		value += "920874760917824938589009714909675985261365549781893129784821"
		value += "682998948722658804857564014270477555132379641451523746234364"
		value += "542858444795265867821051141354735739523113427166102135969536"
		value += "231442952484937187110145765403590279934403742007310578539062"
		value += "198387447808478489683321445713868751943506430218453191048481"
		value += "005370614680674919278191197939952061419663428754440643745123"
		value += "718192179998391015919561814675142691239748940907186494231961"
		value += "567945208095146550225231603881930142093762137855956638937787"
		value += "083039069792077346722182562599661501421503068038447734549202"
		value += "605414665925201497442850732518666002132434088190710486331734"
		value += "649651453905796268561005508106658796998163574736384052571459"
		value += "102897064140110971206280439039759515677157700420337869936007"
		value += "230558763176359421873125147120532928191826186125867321579198"
		value += "414848829164470609575270695722091756711672291098169091528017"
		value += "350671274858322287183520935396572512108357915136988209144421"
		value += "006751033467110314126711136990865851639831501970165151168517"
		value += "143765761835155650884909989859982387345528331635507647918535"
		value += "893226185489632132933089857064204675259070915481416549859461"
		value += "637180270981994309924488957571282890592323326097299712084433"
		value += "573265489382391193259746366730583604142813883032038249037589"
		value += "852437441702913276561809377344403070746921120191302033038019"
		value += "762110110044929321516084244485963766983895228684783123552658"
		value += "213144957685726243344189303968642624341077322697802807318915"
		value += "441101044682325271620105265227211166039666557309254711055785"
		value += "376346682065310989652691862056476931257058635662018558100729"
		value += "360659876486117910453348850346113657686753249441668039626579"
		value += "787718556084552965412665408530614344431858676975145661406800"
		value += "700237877659134401712749470420562230538994561314071127000407"
		value += "854733269939081454664645880797270826683063432858785698305235"
		value += "808933065757406795457163775254202114955761581400250126228594"
		value += "130216471550979259230990796547376125517656751357517829666454"
		value += "779174501129961489030463994713296210734043751895735961458901"
		value += "938971311179042978285647503203198691514028708085990480109412"
		value += "147221317947647772622414254854540332157185306142288137585043"
		value += "063321751829798662237172159160771669254748738986654949450114"
		value += "654062843366393790039769265672146385306736096571209180763832"
		value += "716641627488880078692560290228472104031721186082041900042296"
		value += "617119637792133757511495950156604963186294726547364252308177"
		value += "036751590673502350728354056704038674351362222477158915049530"
		value += "984448933309634087807693259939780541934144737744184263129860"
		value += "809988868741326047215695162396586457302163159819319516735381"
		value += "297416772947867242292465436680098067692823828068996400482435"
		value += "403701416314965897940924323789690706977942236250822168895738"
		value += "379862300159377647165122893578601588161755782973523344604281"
		value += "512627203734314653197777416031990665541876397929334419521541"
		value += "341899485444734567383162499341913181480927777103863877343177"
		value += "207545654532207770921201905166096280490926360197598828161332"
		value += "316663652861932668633606273567630354477628035045077723554710"
		value += "585954870279081435624014517180624643626794561275318134078330"
		value += "336254232783944975382437205835311477119926063813346776879695"
		value += "970309833913077109870408591337464144282277263465947047458784"
		value += "778720192771528073176790770715721344473060570073349243693113"
		value += "835049316312840425121925651798069411352801314701304781643788"
		value += "518529092854520116583934196562134914341595625865865570552690"
		value += "496520985803385072242648293972858478316305777756068887644624"
		value += "824685792603953527734803048029005876075825104747091643961362"
		value += "676044925627420420832085661190625454337213153595845068772460"
		value += "290161876679524061634252257719542916299193064553779914037340"
		value += "432875262888963995879475729174642635745525407909145135711136"
		value += "941091193932519107602082520261879853188770584297259167781314"
		value += "969900901921169717372784768472686084900337702424291651300500"
		value += "516832336435038951702989392233451722013812806965011784408745"
		value += "196012122859937162313017114448464090389064495444006198690754"
		value += "851602632750529834918740786680881833851022833450850486082503"
		value += "930213321971551843063545500766828294930413776552793975175461"
		value += "395398468339363830474611996653858153842056853386218672523340"
		value += "283087112328278921250771262946322956398989893582116745627010"
		value += "218356462201349671518819097303811980049734072396103685406643"
		value += "193950979019069963955245300545058068550195673022921913933918"
		value += "568034490398205955100226353536192041994745538593810234395544"
		value += "959778377902374216172711172364343543947822181852862408514006"
		value += "660443325888569867054315470696574745855033232334210730154594"
		value += "051655379068662733379958511562578432298827372319898757141595"
		value += "781119635833005940873068121602876496286744604774649159950549"
		value += "737425626901049037781986835938146574126804925648798556145372"
		value += "347867330390468838343634655379498641927056387293174872332083"
		value += "760112302991136793862708943879936201629515413371424892830722"
		value += "012690147546684765357616477379467520049075715552781965362132"
		value += "392640616013635815590742202020318727760527721900556148425551"
		value += "879253034351398442532234157623361064250639049750086562710953"
		value += "591946589751413103482276930624743536325691607815478181152843"
		value += "667957061108615331504452127473924544945423682886061340841486"
		value += "377670096120715124914043027253860764823634143346235189757664"
		value += "521641376796903149501910857598442391986291642193994907236234"
		value += "646844117394032659184044378051333894525742399508296591228508"
		value += "555821572503107125701266830240292952522011872676756220415420"
		value += "516184163484756516999811614101002996078386909291603028840026"
		value += "910414079288621507842451670908700069928212066041837180653556"
		value += "725253256753286129104248776182582976515795984703562226293486"
		value += "003415872298053498965022629174878820273420922224533985626476"
		value += "691490556284250391275771028402799806636582548892648802545661"
		value += "017296702664076559042909945681506526530537182941270336931378"
		value += "517860904070866711496558343434769338578171138645587367812301"
		value += "458768712660348913909562009939361031029161615288138437909904"
		value += "231747336394804575931493140529763475748119356709110137751721"
		value += "008031559024853090669203767192203322909433467685142214477379"
		value += "393751703443661991040337511173547191855046449026365512816228"
		value += "824462575916333039107225383742182140883508657391771509682887"
		value += "478265699599574490661758344137522397096834080053559849175417"
		value += "381883999446974867626551658276584835884531427756879002909517"
		value += "028352971634456212964043523117600665101241200659755851276178"
		value += "583829204197484423608007193045761893234922927965019875187212"
		value += "726750798125547095890455635792122103334669749923563025494780"
		value += "249011419521238281530911407907386025152274299581807247162591"
		value += "668545133312394804947079119153267343028244186041426363954800"
		value += "044800267049624820179289647669758318327131425170296923488962"
		value += "766844032326092752496035799646925650493681836090032380929345"
		value += "958897069536534940603402166544375589004563288225054525564056"
		value += "448246515187547119621844396582533754388569094113031509526179"
		value += "378002974120766514793942590298969594699556576121865619673378"
		value += "623625612521632086286922210327488921865436480229678070576561"
		value += "514463204692790682120738837781423356282360896320806822246801"
		value += "224826117718589638140918390367367222088832151375560037279839"
		value += "400415297002878307667094447456013455641725437090697939612257"
		value += "142989467154357846878861444581231459357198492252847160504922"
		value += "124247014121478057345510500801908699603302763478708108175450"
		value += "119307141223390866393833952942578690507643100638351983438934"
		value += "159613185434754649556978103829309716465143840700707360411237"
		value += "359984345225161050702705623526601276484830840761183013052793"
		value += "205427462865403603674532865105706587488225698157936789766974"
		value += "220575059683440869735020141020672358502007245225632651341055"
		value += "924019027421624843914035998953539459094407046912091409387001"
		value += "264560016237428802109276457931065792295524988727584610126483"
		value += "6999892256959688159205600101655256375678566722797"
		
        if NUMBER_OF_DIGITS<10000 then
			value=left(value, NUMBER_OF_DIGITS+9)
		end if
		
		pi_bf=value
		
	end scope
''pi_bf.BigNum = pi_brent_salamin()
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
    g=x
    if g.sign<>0 then
		print "error: argument to Log is negative"
		return zero
	end if
    g.sign=0
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
	if sign<>0 then
		accum=fpdiv(one, accum, dwords)
	end if
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
