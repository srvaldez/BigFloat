#ifdef __FB_64BIT__
	#inclib "DecFloatC"
#else
	#inclib "DecFloatC32"
#endif

#inclib "DecFloatC"

type DecFloat_struct
	Declare Constructor ( )
	Declare Constructor ( Byref rhs As DecFloat_struct )
	Declare Destructor ( )
	Declare Operator Let ( Byref rhs As DecFloat_struct )
	as long sign
	as ulong exponent
	as ulong ptr mantissa
end type 

extern "C"
	declare function memcpy( byval dest as any ptr, byval src as const any ptr, byval count as uinteger ) as any ptr
	declare function memset(byval st as any ptr, byval c as ulong, byval n as uinteger) as any ptr

	Declare Sub DecFloat_Set_Digits(byval digits as long, byval guard_digits as long)
	Declare Sub DecFloat_init(byref as DecFloat_struct)
	Declare Sub DecFloat_clear(byref as DecFloat_struct)
	Declare Function log10_32 (byval as ulong) as long
	Declare Function log10_64 (byval as ulongint) as long
	Declare Function ipower(byval as double, byval as long) as double
	Declare Function fpcmp(byref as DecFloat_struct, byref as DecFloat_struct, byval as long) as long
	Declare Function norm_fac1(byref as DecFloat_struct, byval as long) as long

'	Declare Sub fpadd_aux(byref as DecFloat_struct, byref as DecFloat_struct, byval as long)
'	Declare Sub fpsub_aux(byref as DecFloat_struct, byref as DecFloat_struct, byval as long)
	Declare Sub copydf(byref result as DecFloat_struct, byref source as DecFloat_struct, byval as long)
	Declare Sub fpadd (byref result as DecFloat_struct, byref as DecFloat_struct, byref as DecFloat_struct, byval as long)
	Declare Sub fpsub (byref result as DecFloat_struct, byref as DecFloat_struct, byref as DecFloat_struct, byval as long)
	Declare Sub fpmul (byref result as DecFloat_struct, byref as DecFloat_struct, byref as DecFloat_struct, byval as long)
	Declare Sub fpdiv (byref result as DecFloat_struct, byref as DecFloat_struct, byref as DecFloat_struct, byval as long)
	Declare Sub si2fp(byref result as DecFloat_struct, byval as longint, byval as long)
	Declare Sub ui2fp(byref result as DecFloat_struct, byval as ulongint, byval as long)
	declare function fp2i(byref x as DecFloat_struct) as long
	declare function fp2i64(byref x as DecFloat_struct) as longint
	declare function fp2si alias "fp2i"(byref x as DecFloat_struct) as long
	declare function fp2ui(byref x as DecFloat_struct) as ulong
	declare function fp2ui64(byref x as DecFloat_struct) as ulongint
	declare sub dbl2fp (byref result as DecFloat_struct, byval x as double, byval dwords as long)
	declare function fp2dbl(byref x as DecFloat_struct) as double
	declare sub fpipow(byref result as DecFloat_struct, byref x as DecFloat_struct, byval e as longint, byval dwords as long)
	declare sub fpfix(byref ip as DecFloat_struct, byref num as DecFloat_struct, byval dwords as long)
	declare sub fpfrac(byref fp as DecFloat_struct, byref num as DecFloat_struct, byval dwords as long)
	declare sub fpexp_aux (byref result as DecFloat_struct, byref x as DecFloat_struct, byval dwords as long)
	declare sub fpexp (byref result as DecFloat_struct, byref x as DecFloat_struct, byval dwords as long)
	declare sub fpdiv_si(byref result as DecFloat_struct, byref num as DecFloat_struct, byval den as long, byval dwords as long) 
	declare sub fpsqr(byref result as DecFloat_struct, byref x as DecFloat_struct, byval dwords as long)
	declare sub fpmul_si(byref result as DecFloat_struct, byref x as DecFloat_struct, byval y as longint, byval dwords as long)
	declare sub pi_chudnovsky_bs(byref pi as DecFloat_struct, byval digits as long )
	declare sub fplog_agm (byref result as DecFloat_struct, byref x as DecFloat_struct, byval dwords as long)
	declare sub fplog10 (byref result as DecFloat_struct, byref x as DecFloat_struct, byval dwords as long)
	''declare sub DecFloat_Clear_Constants()
	declare sub Pi_RamanujanBS (Byref pi as DecFloat_struct, byval dwords as long)
	declare sub fpsin (byref result as DecFloat_struct, byref x as DecFloat_struct, byval dwords as long)
	declare sub fpcos (byref result as DecFloat_struct, byref x as DecFloat_struct, byval dwords as long)
	declare sub fptan (byref result as DecFloat_struct, byref x as DecFloat_struct, byval dwords as long)
	
	declare sub fpatn (byref result as DecFloat_struct, byref x as DecFloat_struct, byval dwords as long)
	declare sub fpasin (byref result as DecFloat_struct, byref x as DecFloat_struct, byval dwords as long)
	declare sub fpacos (byref result as DecFloat_struct, byref x as DecFloat_struct, byval dwords as long)
	declare sub agm(byref result as DecFloat_struct, byref a as DecFloat_struct, byref b as DecFloat_struct, byval digits as long)
	''declare sub fprndup(byref fac1 as DecFloat_struct, byval carry as ulongint, byval dwords as long)
	declare sub get_pi_df(byref result as DecFloat_struct)
	declare sub get_pi2_df(byref result as DecFloat_struct)
	declare sub get_pi_half_df(byref result as DecFloat_struct)
	declare sub get_ln2_df(byref result as DecFloat_struct)
	declare sub get_ln10_df(byref result as DecFloat_struct)
	declare sub get_Log10e_df(byref result as DecFloat_struct)
	declare sub get_exp1_df(byref result as DecFloat_struct)

	declare sub fp2str2 alias "fp2str"(byval result as zstring ptr, byref z as DecFloat_struct, byval digits as long)
	declare sub str2fp2 alias "str2fp"(byref result as DecFloat_struct, byval value_in as zstring ptr)
	extern NUMBER_OF_DIGITS as long
	extern NUMBER_OF_BITS as long
	extern NUM_DWORDS as long
	extern NUM_BYTES as long
	extern BIAS as const long
	
end extern

Constructor DecFloat_struct ( )
	DecFloat_init(this)
End Constructor

Constructor DecFloat_struct ( Byref rhs As DecFloat_struct )
	DecFloat_init(this)
	copydf(this, rhs, NUM_DWORDS)
End Constructor

Operator DecFloat_struct.let ( Byref rhs As DecFloat_struct )
	copydf(this, rhs, NUM_DWORDS)
End Operator

Destructor DecFloat_struct ( )
	DecFloat_clear(this)
End Destructor

sub str2fp(byref z As DecFloat_struct, Byref x As String, byval dwords as long=NUM_DWORDS)
	dim as zstring ptr zs=callocate(len(x)+20)
	*zs=x
	str2fp2(z, zs)
	deallocate(zs)
End sub

Function fp2str(Byref z As DecFloat_struct, Byval digits As Long=NUMBER_OF_DIGITS) As String
	If digits>=(NUMBER_OF_DIGITS) Or digits<0 Then digits=NUMBER_OF_DIGITS+2
	dim as zstring ptr zs=callocate(digits+20)
	dim as string s
	
	fp2str2(zs, z, digits)
	s=*zs
	deallocate(zs)
	Return s
End Function


Type DecFloat
	Declare Constructor ( )
	Declare Constructor ( Byval rhs As Long )
	Declare Constructor ( Byval rhs As Integer )
	Declare Constructor ( Byval rhs As LongInt )
	Declare Constructor ( Byval rhs As Single )
	Declare Constructor ( Byval rhs As Double )
	Declare Constructor ( Byref rhs As String )
	Declare Constructor ( Byref rhs As DecFloat )
	Declare Destructor ( )
	
	Declare Operator Let ( Byval rhs As Long )
	Declare Operator Let ( Byval rhs As LongInt )
	Declare Operator Let ( Byval rhs As Integer )
	Declare Operator Let ( Byval rhs As Single )
	Declare Operator Let ( Byval rhs As Double )
	Declare Operator Let ( Byref rhs As String )
	Declare Operator Let ( Byref rhs As DecFloat )
	Declare Operator Cast ( ) As String
	Declare Operator Cast ( ) As Long
	Declare Operator Cast ( ) As uLong
	Declare Operator Cast ( ) As Longint
	Declare Operator Cast ( ) As uLongint
	Declare Operator Cast ( ) As Double
	
	'----------------------------------------------
	Declare Operator += (Byref rhs As DecFloat)
	Declare Operator += (Byval rhs As Long)
	Declare Operator += (Byval rhs As Double)
	Declare Operator += (Byref rhs As String)
	Declare Operator -= (Byref rhs As DecFloat)
	Declare Operator -= (Byval rhs As Long)
	Declare Operator -= (Byval rhs As Double)
	Declare Operator -= (Byref rhs As String)
	Declare Operator *= (Byref rhs As DecFloat)
	Declare Operator *= (Byval rhs As Long)
	Declare Operator *= (Byval rhs As Double)
	Declare Operator *= (Byref rhs As String)
	Declare Operator /= (Byref rhs As DecFloat)
	Declare Operator /= (Byval rhs As Long)
	Declare Operator /= (Byval rhs As Double)
	Declare Operator /= (Byval rhs As Single)
	Declare Operator /= (Byref rhs As String)

	' For Next Implicit step = +1
	Declare Operator For ( )
	Declare Operator Step( )
	Declare Operator Next( Byref end_cond As DecFloat ) As Integer
	' For Next Exlicit step
	Declare Operator For ( Byref stp As DecFloat )
	Declare Operator Step( Byref stp As DecFloat )
	Declare Operator Next( Byref end_cond As DecFloat, Byref step_var As DecFloat ) As Integer
	
	Declare Function toString( Byval places As Long=NUM_DWORDS*9.63 ) As String
	Declare Function toLong ( ) As Long
	Declare Function toDouble ( ) As Double

	DecNum As DecFloat_struct

End Type

Function Decfloat.toString( Byval places As Long=NUM_DWORDS*9.63 ) As String
	Function = fp2str( this.DecNum, places )
End Function

Function Decfloat.toLong ( ) As Long
	Dim As Double x
	x=fp2dbl(this.DecNum)
	Return Clng(x)
End Function


Function Decfloat.toDouble ( ) As Double
	Function = fp2dbl(this.DecNum)
End Function

Constructor Decfloat ( )
	si2fp(this.DecNum, 0, NUM_DWORDS)
End Constructor

Constructor Decfloat ( Byval rhs As Long )
	si2fp(this.DecNum, rhs, NUM_DWORDS )
End Constructor

Constructor Decfloat ( Byval rhs As LongInt )
	si2fp(this.DecNum, rhs, NUM_DWORDS )
End Constructor

Constructor Decfloat ( Byval rhs As Integer )
	#ifdef __FB_64BIT__
		si2fp(this.DecNum, rhs, NUM_DWORDS )
	#else
		si2fp(this.DecNum, rhs, NUM_DWORDS )
	#endif
End Constructor

Constructor Decfloat ( Byval rhs As Single )
	dbl2fp(this.DecNum, cdbl(rhs), NUM_DWORDS )
End Constructor

Constructor Decfloat ( Byval rhs As Double )
	dbl2fp(this.DecNum, rhs, NUM_DWORDS )
End Constructor

Constructor Decfloat ( Byref rhs As String )
	str2fp(this.DecNum, rhs )
End Constructor

Constructor Decfloat ( Byref rhs As Decfloat)
	this.DecNum.sign=rhs.DecNum.sign
	this.DecNum.exponent=rhs.DecNum.exponent
	memcpy( this.DecNum.mantissa, rhs.DecNum.mantissa, NUM_BYTES )
End Constructor

Operator Decfloat.let ( Byref rhs As Decfloat )
	this.DecNum.sign=rhs.DecNum.sign
	this.DecNum.exponent=rhs.DecNum.exponent
	memcpy( this.DecNum.mantissa, rhs.DecNum.mantissa, NUM_BYTES )
End Operator

Operator Decfloat.let ( Byval rhs As Long )
	si2fp(this.DecNum, rhs, NUM_DWORDS )
End Operator

Operator Decfloat.let ( Byval rhs As LongInt )
	si2fp(this.DecNum, rhs, NUM_DWORDS  )
End Operator

Operator Decfloat.let ( Byval rhs As Integer )
	#ifdef __FB_64BIT__
		si2fp(this.DecNum, rhs, NUM_DWORDS  )
	#else
		si2fp(this.DecNum, rhs, NUM_DWORDS )
	#endif
End Operator

Operator Decfloat.Let ( Byval rhs As single )
	dbl2fp(this.DecNum, cdbl(rhs), NUM_DWORDS  )
End Operator

Operator Decfloat.Let ( Byval rhs As Double )
	dbl2fp(this.DecNum, rhs, NUM_DWORDS  )
End Operator

Operator Decfloat.let ( Byref rhs As String )
	str2fp(this.DecNum, rhs )
End Operator

Operator Decfloat.cast ( ) As String
	Operator = fp2str(this.DecNum)
End Operator

Operator Decfloat.cast ( ) As Long
	operator=fp2si(this.DecNum)
End Operator

Operator Decfloat.cast ( ) As uLong
	Operator = fp2ui(this.DecNum)
End Operator

Operator Decfloat.cast ( ) As Longint
	Operator = fp2i64(this.DecNum )
End Operator

Operator Decfloat.cast ( ) As uLongint
	Operator = fp2i64(this.DecNum)
End Operator

Operator Decfloat.cast ( ) As Double
	Operator = fp2dbl(this.DecNum)
End Operator

Destructor Decfloat ( )
End Destructor

'============================================================================
Operator DecFloat.for ( )
End Operator
 
Operator DecFloat.step ( )
	dim as DecFloat_struct tmp
	si2fp(tmp, 1, NUM_DWORDS)
	fpadd(this.DecNum, this.DecNum, tmp, NUM_DWORDS)
End Operator 
 
Operator DecFloat.next ( Byref end_cond As DecFloat ) As Integer
	Return fpcmp(This.DecNum, end_cond.DecNum, NUM_DWORDS)<=0
End Operator
 
 
'' explicit step versions
'' 
Operator DecFloat.for ( Byref step_var As DecFloat )
End Operator
 
Operator DecFloat.step ( Byref step_var As DecFloat )
	fpadd(this.DecNum, this.DecNum, step_var.DecNum, NUM_DWORDS)	
End Operator 
 
Operator DecFloat.next ( Byref end_cond As DecFloat, Byref step_var As DecFloat ) As Integer
	dim as DecFloat_struct tmp
	si2fp(tmp, 0, NUM_DWORDS)
	If fpcmp(step_var.DecNum, tmp, NUM_DWORDS) < 0 Then
		Return fpcmp(This.DecNum, end_cond.DecNum, NUM_DWORDS) >= 0
	Else
		Return fpcmp(This.DecNum, end_cond.DecNum, NUM_DWORDS) <= 0
	End If
End Operator

'============================================================================

Operator + ( Byref lhs As Decfloat, Byval rhs As Decfloat ) As Decfloat
	Dim As Decfloat result
	fpadd(result.DecNum, lhs.DecNum, rhs.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator + ( Byref lhs As Decfloat, Byval rhs As Double ) As Decfloat
	Dim As Decfloat result
	dbl2fp(result.DecNum, rhs, NUM_DWORDS )
	fpadd(result.DecNum, lhs.DecNum, result.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator + ( Byval lhs As Double, Byref rhs As Decfloat ) As Decfloat
	Dim As Decfloat result
	dbl2fp(result.DecNum, lhs, NUM_DWORDS )
	fpadd(result.DecNum, result.DecNum, rhs.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator + ( Byref lhs As Decfloat, Byval rhs As Long ) As Decfloat
	Dim As Decfloat result
	si2fp(result.DecNum, rhs, NUM_DWORDS )
	fpadd(result.DecNum, lhs.DecNum, result.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator + ( Byval lhs As Long, Byref rhs As Decfloat ) As Decfloat
	Dim As Decfloat result
	si2fp(result.DecNum, lhs, NUM_DWORDS )
	fpadd(result.DecNum, result.DecNum, rhs.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator + (Byref lhs As Decfloat, Byref rhs As String) As Decfloat
	Dim As Decfloat result
	str2fp(result.DecNum, rhs)
	fpadd(result.DecNum, lhs.DecNum, result.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator - ( Byref rhs As Decfloat ) As Decfloat
	Dim As Decfloat result = rhs
	result.DecNum.sign = result.DecNum.sign Xor (-1)
	Operator = result
End Operator

Operator - ( Byref lhs As Decfloat, Byval rhs As Decfloat ) As Decfloat
	Dim As Decfloat result
	fpsub(result.DecNum, lhs.DecNum, rhs.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator - ( Byref lhs As Decfloat, Byval rhs As Double ) As Decfloat
	Dim As Decfloat result
	dbl2fp(result.DecNum, rhs, NUM_DWORDS )
	fpsub(result.DecNum, lhs.DecNum, result.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator - ( Byval lhs As Double, Byref rhs As Decfloat ) As Decfloat
	Dim As Decfloat result
	dbl2fp(result.DecNum, lhs, NUM_DWORDS )
	fpsub(result.DecNum, result.DecNum, rhs.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator - ( Byref lhs As Decfloat, Byval rhs As Long ) As Decfloat
	Dim As Decfloat result
	si2fp(result.DecNum, rhs, NUM_DWORDS )
	fpsub(result.DecNum, lhs.DecNum, result.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator - ( Byval lhs As Long, Byref rhs As Decfloat ) As Decfloat
	Dim As Decfloat result
	si2fp(result.DecNum, lhs, NUM_DWORDS )
	fpsub(result.DecNum, result.DecNum, rhs.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator - (Byref lhs As Decfloat, Byref rhs As String) As Decfloat
	Dim As Decfloat result
	str2fp(result.DecNum, rhs)
	fpsub(result.DecNum, lhs.DecNum, result.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator * ( Byref lhs As Decfloat, Byval rhs As Decfloat ) As Decfloat
	Dim As Decfloat result
	fpmul(result.DecNum, lhs.DecNum, rhs.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator * ( Byref lhs As Decfloat, Byval rhs As Double ) As Decfloat
	Dim As Decfloat result
	dbl2fp(result.DecNum, rhs, NUM_DWORDS )
	fpmul(result.DecNum, lhs.DecNum, result.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator * ( Byval lhs As Double, Byref rhs As Decfloat ) As Decfloat
	Dim As Decfloat result
	dbl2fp(result.DecNum, lhs, NUM_DWORDS )
	fpmul(result.DecNum, result.DecNum, rhs.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator * ( Byref lhs As Decfloat, Byval rhs As Long ) As Decfloat
	Dim As Decfloat result
	si2fp(result.DecNum, rhs, NUM_DWORDS )
	fpmul(result.DecNum, lhs.DecNum, result.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator * ( Byval lhs As Long, Byref rhs As Decfloat ) As Decfloat
	Dim As Decfloat result
	si2fp(result.DecNum, lhs, NUM_DWORDS )
	fpmul(result.DecNum, result.DecNum, rhs.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator * (Byref lhs As Decfloat, Byref rhs As String) As Decfloat
	Dim As Decfloat result
	str2fp(result.DecNum, rhs)
	fpmul(result.DecNum, lhs.DecNum, result.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator / ( Byref lhs As Decfloat, Byval rhs As Decfloat ) As Decfloat
	Dim As Decfloat result
	fpdiv(result.DecNum, lhs.DecNum, rhs.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator / ( Byref lhs As Decfloat, Byval rhs As Double ) As Decfloat
	Dim As Decfloat result
	dbl2fp(result.DecNum, rhs, NUM_DWORDS )
	fpdiv(result.DecNum, lhs.DecNum, result.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator / ( Byval lhs As Double, Byref rhs As Decfloat ) As Decfloat
	Dim As Decfloat result
	dbl2fp(result.DecNum, lhs, NUM_DWORDS )
	fpdiv(result.DecNum, result.DecNum, rhs.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator / ( Byref lhs As Decfloat, Byval rhs As Long ) As Decfloat
	Dim As Decfloat result
	si2fp(result.DecNum, rhs, NUM_DWORDS )
	fpdiv(result.DecNum, lhs.DecNum, result.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator / ( Byval lhs As Long, Byref rhs As Decfloat ) As Decfloat
	Dim As Decfloat result
	si2fp(result.DecNum, lhs, NUM_DWORDS )
	fpdiv(result.DecNum, result.DecNum, rhs.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator / (Byref lhs As Decfloat, Byref rhs As String) As Decfloat
	Dim As Decfloat result
	str2fp(result.DecNum, rhs)
	fpdiv(result.DecNum, lhs.DecNum, result.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator ^ ( Byref lhs As Decfloat, Byval rhs As Longint ) As Decfloat
	Dim As Decfloat lhs2
	fpipow(lhs2.DecNum, lhs.DecNum, rhs, NUM_DWORDS)
	Operator = lhs2
End Operator

Operator ^ ( Byref lhs As Decfloat, Byref rhs As Decfloat ) As Decfloat
	Dim As Decfloat lhs2
	fplog_agm(lhs2.DecNum, lhs.DecNum, NUM_DWORDS)
	fpmul(lhs2.DecNum, lhs2.DecNum, rhs.DecNum, NUM_DWORDS)
	fpexp(lhs2.DecNum, lhs2.DecNum, NUM_DWORDS)
	Operator = lhs2
End Operator

Operator DecFloat.+= (Byref rhs As DecFloat)
	fpadd(this.DecNum, this.DecNum, rhs.DecNum, NUM_DWORDS)
End Operator

Operator DecFloat.+= (Byval rhs As Long)
	Dim As DecFloat result
	si2fp(result.DecNum, rhs, NUM_DWORDS)
	fpadd(this.DecNum, this.DecNum, result.DecNum, NUM_DWORDS)
End Operator

Operator DecFloat.+= (Byval rhs As Double)
	Dim As DecFloat result
	dbl2fp(result.DecNum, rhs, NUM_DWORDS )
	fpadd(this.DecNum, this.DecNum, result.DecNum, NUM_DWORDS)
End Operator

Operator DecFloat.+= (Byref rhs As String)
	Dim As DecFloat result
	str2fp(result.DecNum, rhs)
	fpadd(this.DecNum, this.DecNum, result.DecNum, NUM_DWORDS)
End Operator

Operator DecFloat.-= (Byref rhs As DecFloat)
	fpsub(this.DecNum, this.DecNum, rhs.DecNum, NUM_DWORDS)
End Operator

Operator DecFloat.-= (Byval rhs As Long)
	Dim As DecFloat result
	si2fp(result.DecNum, rhs, NUM_DWORDS)
	fpsub(this.DecNum, this.DecNum, result.DecNum, NUM_DWORDS)
End Operator

Operator DecFloat.-= (Byval rhs As Double)
	Dim As DecFloat result
	dbl2fp(result.DecNum, rhs, NUM_DWORDS )
	fpsub(this.DecNum, this.DecNum, result.DecNum, NUM_DWORDS)
End Operator

Operator DecFloat.-= (Byref rhs As String)
	Dim As DecFloat result
	str2fp(result.DecNum, rhs)
	fpsub(this.DecNum, this.DecNum, result.DecNum, NUM_DWORDS)
End Operator

Operator DecFloat.*= (Byref rhs As DecFloat)
	fpmul(this.DecNum, this.DecNum, rhs.DecNum, NUM_DWORDS)
End Operator

Operator DecFloat.*= (Byval rhs As Long)
	Dim As DecFloat result
	si2fp(result.DecNum, rhs, NUM_DWORDS)
	fpmul(this.DecNum, this.DecNum, result.DecNum, NUM_DWORDS)
End Operator

Operator DecFloat.*= (Byval rhs As Double)
	Dim As DecFloat result
	dbl2fp(result.DecNum, rhs, NUM_DWORDS )
	fpmul(this.DecNum, this.DecNum, result.DecNum, NUM_DWORDS)
End Operator

Operator DecFloat.*= (Byref rhs As String)
	Dim As DecFloat result
	str2fp(result.DecNum, rhs)
	fpmul(this.DecNum, this.DecNum, result.DecNum, NUM_DWORDS)
End Operator

Operator DecFloat./= (Byref rhs As DecFloat)
	fpdiv(this.DecNum, this.DecNum, rhs.DecNum, NUM_DWORDS)
End Operator

Operator DecFloat./= (Byval rhs As Long)
	Dim As DecFloat result
	si2fp(result.DecNum, rhs, NUM_DWORDS)
	fpdiv(this.DecNum, this.DecNum, result.DecNum, NUM_DWORDS)
End Operator

Operator DecFloat./= (Byval rhs As Double)
	Dim As DecFloat result
	dbl2fp(result.DecNum, rhs, NUM_DWORDS )
	fpdiv(this.DecNum, this.DecNum, result.DecNum, NUM_DWORDS)
End Operator

Operator DecFloat./= (Byref rhs As String)
	Dim As DecFloat result
	str2fp(result.DecNum, rhs)
	fpdiv(this.DecNum, this.DecNum, result.DecNum, NUM_DWORDS)
End Operator

Operator = ( Byref lhs As DecFloat, Byref rhs As DecFloat ) As Long
	Operator = fpcmp(lhs.DecNum, rhs.DecNum, NUM_DWORDS)=0
End Operator

Operator < ( Byref lhs As DecFloat, Byref rhs As DecFloat ) As Long
	Operator = fpcmp(lhs.DecNum, rhs.DecNum, NUM_DWORDS)<0
End Operator

Operator > ( Byref lhs As DecFloat, Byref rhs As DecFloat ) As Long
	Operator = fpcmp(lhs.DecNum, rhs.DecNum, NUM_DWORDS)>0
End Operator

Operator <= ( Byref lhs As DecFloat, Byref rhs As DecFloat ) As Long
	Operator = fpcmp(lhs.DecNum, rhs.DecNum, NUM_DWORDS)<=0
End Operator

Operator >= ( Byref lhs As DecFloat, Byref rhs As DecFloat ) As Long
	Operator = fpcmp(lhs.DecNum, rhs.DecNum, NUM_DWORDS)>=0
End Operator

Operator <> ( Byref lhs As DecFloat, Byref rhs As DecFloat ) As Long
	Operator = fpcmp(lhs.DecNum, rhs.DecNum, NUM_DWORDS)<>0
End Operator

Operator Abs(Byref rhs As DecFloat) As DecFloat
	Dim As DecFloat result
	result.DecNum=rhs.DecNum
	result.DecNum.sign = 0
	Operator = result
End Operator

Operator  Fix (Byref x As DecFloat) As DecFloat
	Dim As DecFloat result
	fpfix(result.DecNum , x.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator Frac(Byref x As DecFloat) As DecFloat
	Dim As DecFloat result
	fpfrac(result.DecNum, x.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator Sqr(Byref x As DecFloat) As DecFloat
	Dim As DecFloat result
	fpsqr(result.DecNum, x.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator Sin(Byref x As DecFloat) As DecFloat
	Dim As DecFloat result
	fpsin(result.DecNum, x.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator Cos(Byref x As DecFloat) As DecFloat
	Dim As DecFloat result
	fpcos(result.DecNum, x.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator Tan(Byref x As DecFloat) As DecFloat
	Dim As DecFloat result
	fptan(result.DecNum, x.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator Atn(Byref x As DecFloat) As DecFloat
	Dim As DecFloat result
	fpatn(result.DecNum, x.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator Asin(Byref x As DecFloat) As DecFloat
	Dim As DecFloat result
	fpasin(result.DecNum, x.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator Acos(Byref x As DecFloat) As DecFloat
	Dim As DecFloat result
	fpacos(result.DecNum, x.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator Exp(Byref x As DecFloat) As DecFloat
	Dim As DecFloat result
	fpexp(result.DecNum, x.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator Log(Byref x As DecFloat) As DecFloat
	Dim As DecFloat result
	fplog_agm(result.DecNum, x.DecNum, NUM_DWORDS)
	Operator = result
End Operator

Operator sgn(Byref x As DecFloat) As long
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

Function fphex(ByRef x As DecFloat, byval dwords As long = NUM_DWORDS) As String
	If dwords > NUM_DWORDS Then dwords = NUM_DWORDS
	Dim As String s, s2
	Dim As long i, ex

	s2 = ""
	s = Hex(x.DecNum.mantissa[0])
	If Len(s) < 8 Then
		s = String(8 - Len(s), "0") + s
	End If
	s2 = "0X" + Left(s, 1) + "." + Mid(s, 2)
	For i = 1 To NUM_DWORDS
		s = Hex(x.DecNum.mantissa[i])
		If Len(s) < 8 Then
			s = String(8 - Len(s), "0") + s
		End If
		s2 += s
	Next
	If x.DecNum.exponent <> 0 Then
		ex = (x.DecNum.exponent And &H7FFFFFFF) - BIAS - 1
	Else
		ex = 0
	End If
	s2 = s2 + "P" + Str(ex)
	Return s2
End Function
