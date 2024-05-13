#pragma once

#include once "crt/long.bi"

#inclib "quadmath"
#inclib "quadmath_arith"

Type __float128
	as ulong mq(3)
    Declare Constructor ( )
    Declare Constructor ( Byval rhs As Long )
    Declare Constructor ( Byval rhs As Longint )
    Declare Constructor ( Byval rhs As Integer )
    Declare Constructor ( Byval rhs As Double )
    Declare Constructor ( Byval rhs As Single )
    Declare Constructor ( Byref rhs As String )
    Declare constructor ( Byref rhs As __float128 )

    Declare Destructor ( )

    Declare Operator Let ( Byval rhs As Long )
    Declare Operator Let ( Byval rhs As Longint )
    Declare Operator Let ( Byval rhs As Integer )
    Declare Operator Let ( Byval rhs As Double )
    Declare Operator Let ( Byval rhs As Single )
    Declare Operator Let ( Byref rhs As String )
    Declare Operator Let ( Byref rhs As __float128 )
    Declare Operator Cast ( ) As String
    Declare Operator Cast ( ) As Long
    Declare Operator Cast ( ) As Double
    Declare Operator Cast ( ) As Single

    '----------------------------------------------
    Declare Operator += (Byref rhs As __float128)
    Declare Operator += (Byval rhs As Longint)
    Declare Operator += (Byval rhs As Double)
    Declare Operator += (Byval rhs As Single)
    Declare Operator += (Byref rhs As String)
    Declare Operator -= (Byref rhs As __float128)
    Declare Operator -= (Byval rhs As Longint)
    Declare Operator -= (Byval rhs As Double)
    Declare Operator -= (Byval rhs As Single)
    Declare Operator -= (Byref rhs As String)
    Declare Operator *= (Byref rhs As __float128)
    Declare Operator *= (Byval rhs As Longint)
    Declare Operator *= (Byval rhs As Double)
    Declare Operator *= (Byval rhs As Single)
    Declare Operator *= (Byref rhs As String)
    Declare Operator /= (Byref rhs As __float128)
    Declare Operator /= (Byval rhs As Longint)
    Declare Operator /= (Byval rhs As Double)
    Declare Operator /= (Byval rhs As Single)
    Declare Operator /= (Byref rhs As String)
    Declare Operator ^= ( Byref rhs As __float128)
    Declare Operator ^= (Byval rhs As Long)
    Declare Operator ^= (Byval rhs As uLong)

    ' For Next Implicit step = +1
    Declare Operator For ( )
    Declare Operator Step( )
    Declare Operator Next( Byref end_cond As __float128 ) As Integer
    ' For Next Exlicit step
    Declare Operator For ( Byref stp As __float128 )
    Declare Operator Step( Byref stp As __float128 )
    Declare Operator Next( Byref end_cond As __float128, Byref step_var As __float128 ) As Integer
    '----------------------------------------------
    Declare Function toString( ) As String
    Declare Function toLong ( ) As Long
    Declare Function toDouble ( ) As Double
    Declare Function toSingle ( ) As Single

End Type

	type __complex128
		re as __float128
		im as __float128
		Declare constructor (  )
		Declare constructor ( Byref rhs As __complex128 )
		Declare destructor (  )
	end type

#define QUADMATH_H

extern "C"
	declare function acosq(byval as __float128) as __float128
	declare function acoshq(byval as __float128) as __float128
	declare function asinq(byval as __float128) as __float128
	declare function asinhq(byval as __float128) as __float128
	declare function atanq(byval as __float128) as __float128
	declare function atanhq(byval as __float128) as __float128
	declare function atan2q(byval as __float128, byval as __float128) as __float128
	declare function cbrtq(byval as __float128) as __float128
	declare function ceilq(byval as __float128) as __float128
	declare function copysignq(byval as __float128, byval as __float128) as __float128
	declare function coshq(byval as __float128) as __float128
	declare function cosq(byval as __float128) as __float128
	declare function erfq(byval as __float128) as __float128
	declare function erfcq(byval as __float128) as __float128
	declare function exp2q(byval as __float128) as __float128
	declare function expq(byval as __float128) as __float128
	declare function expm1q(byval as __float128) as __float128
	declare function fabsq(byval as __float128) as __float128
	declare function fdimq(byval as __float128, byval as __float128) as __float128
	declare function finiteq(byval as __float128) as long
	declare function floorq(byval as __float128) as __float128
	declare function fmaq(byval as __float128, byval as __float128, byval as __float128) as __float128
	declare function fmaxq(byval as __float128, byval as __float128) as __float128
	declare function fminq(byval as __float128, byval as __float128) as __float128
	declare function fmodq(byval as __float128, byval as __float128) as __float128
	declare function frexpq(byval as __float128, byval as long ptr) as __float128
	declare function hypotq(byval as __float128, byval as __float128) as __float128
	declare function isinfq(byval as __float128) as long
	declare function ilogbq(byval as __float128) as long
	declare function isnanq(byval as __float128) as long
	declare function issignalingq(byval as __float128) as long
	declare function j0q(byval as __float128) as __float128
	declare function j1q(byval as __float128) as __float128
	declare function jnq(byval as long, byval as __float128) as __float128
	declare function ldexpq(byval as __float128, byval as long) as __float128
	declare function lgammaq(byval as __float128) as __float128
	declare function llrintq(byval as __float128) as longint
	declare function llroundq(byval as __float128) as longint
	declare function logbq(byval as __float128) as __float128
	declare function logq(byval as __float128) as __float128
	declare function log10q(byval as __float128) as __float128
	declare function log2q(byval as __float128) as __float128
	declare function log1pq(byval as __float128) as __float128
	declare function lrintq(byval as __float128) as clong
	declare function lroundq(byval as __float128) as clong
	declare function modfq(byval as __float128, byval as __float128 ptr) as __float128
	declare function nanq(byval as const zstring ptr) as __float128
	declare function nearbyintq(byval as __float128) as __float128
	declare function nextafterq(byval as __float128, byval as __float128) as __float128
	declare function powq(byval as __float128, byval as __float128) as __float128
	declare function remainderq(byval as __float128, byval as __float128) as __float128
	declare function remquoq(byval as __float128, byval as __float128, byval as long ptr) as __float128
	declare function rintq(byval as __float128) as __float128
	declare function roundq(byval as __float128) as __float128
	declare function scalblnq(byval as __float128, byval as clong) as __float128
	declare function scalbnq(byval as __float128, byval as long) as __float128
	declare function signbitq(byval as __float128) as long
	declare sub sincosq(byval as __float128, byval as __float128 ptr, byval as __float128 ptr)
	declare function sinhq(byval as __float128) as __float128
	declare function sinq(byval as __float128) as __float128
	declare function sqrtq(byval as __float128) as __float128
	declare function tanq(byval as __float128) as __float128
	declare function tanhq(byval as __float128) as __float128
	declare function tgammaq(byval as __float128) as __float128
	declare function truncq(byval as __float128) as __float128
	declare function y0q(byval as __float128) as __float128
	declare function y1q(byval as __float128) as __float128
	declare function ynq(byval as long, byval as __float128) as __float128
	declare function cabsq(byval as __complex128) as __float128
	declare function cargq(byval as __complex128) as __float128
	declare function cimagq(byval as __complex128) as __float128
	declare function crealq(byval as __complex128) as __float128
	declare function cacosq(byval as __complex128) as __complex128
	declare function cacoshq(byval as __complex128) as __complex128
	declare function casinq(byval as __complex128) as __complex128
	declare function casinhq(byval as __complex128) as __complex128
	declare function catanq(byval as __complex128) as __complex128
	declare function catanhq(byval as __complex128) as __complex128
	declare function ccosq(byval as __complex128) as __complex128
	declare function ccoshq(byval as __complex128) as __complex128
	declare function cexpq(byval as __complex128) as __complex128
	declare function cexpiq(byval as __float128) as __complex128
	declare function clogq(byval as __complex128) as __complex128
	declare function clog10q(byval as __complex128) as __complex128
	declare function conjq(byval as __complex128) as __complex128
	declare function cpowq(byval as __complex128, byval as __complex128) as __complex128
	declare function cprojq(byval as __complex128) as __complex128
	declare function csinq(byval as __complex128) as __complex128
	declare function csinhq(byval as __complex128) as __complex128
	declare function csqrtq(byval as __complex128) as __complex128
	declare function ctanq(byval as __complex128) as __complex128
	declare function ctanhq(byval as __complex128) as __complex128
	declare function strtoflt128(byval as const zstring ptr, byval as zstring ptr ptr) as __float128
	declare function quadmath_snprintf(byval str as zstring ptr, byval size as uinteger, byval format as const zstring ptr, as __float128) as long



	private function cimagq(byval __z as __complex128) as __float128
		return __z.im
	end function

	private function crealq(byval __z as __complex128) as __float128
		return __z.re
	end function

	declare function negq(byref as __float128) as __float128
	declare function addq(byref as __float128, byref as __float128) as __float128
	declare function addq_double(byref as __float128, byval as double) as __float128
	declare function double_addq(byval as double, byref as __float128) as __float128
	declare function addq_i32(byref as __float128, byval as long) as __float128
	declare function addq_u32(byref as __float128, byval as ulong) as __float128
	declare function addq_i64(byref as __float128, byval as longint) as __float128
	declare function i64_addq(byval as longint, byref as __float128) as __float128
	declare function subq(byref as __float128, byref as __float128) as __float128
	declare function subq_double(byref as __float128, byval as double) as __float128
	declare function double_subq(byval as double, byref as __float128) as __float128
	declare function subq_i32(byref as __float128, byval as long) as __float128
	declare function subq_u32(byref as __float128, byval as ulong) as __float128
	declare function subq_i64(byref as __float128, byval as longint) as __float128
	declare function i64_subq(byval as longint, byref as __float128) as __float128
	declare function mulq(byref as __float128, byref as __float128) as __float128
	declare function mulq_double(byref as __float128, byval as double) as __float128
	declare function double_mulq(byval as double, byref as __float128) as __float128
	declare function mulq_i32(byref as __float128, byval as long) as __float128
	declare function mulq_u32(byref as __float128, byval as ulong) as __float128
	declare function mulq_i64(byref as __float128, byval as longint) as __float128
	declare function i64_mulq(byval as longint, byref as __float128) as __float128
	declare function divq(byref as __float128, byref as __float128) as __float128
	declare function divq_double(byref as __float128, byval as double) as __float128
	declare function double_divq(byval as double, byref as __float128) as __float128
	declare function divq_i32(byref as __float128, byval as long) as __float128
	declare function divq_u32(byref as __float128, byval as ulong) as __float128
	declare function divq_i64(byref as __float128, byval as longint) as __float128
	declare function i64_divq(byval as longint, byref as __float128) as __float128
	
	declare function i32_addq(byval as long, byref as __float128) as __float128
	declare function u32_addq(byval as ulong, byref as __float128) as __float128
	declare function i32_subq(byval as long, byref as __float128) as __float128
	declare function u32_subq(byval as ulong, byref as __float128) as __float128
	declare function i32_mulq(byval as long, byref as __float128) as __float128
	declare function u32_mulq(byval as ulong, byref as __float128) as __float128
	declare function i32_divq(byval as long, byref as __float128) as __float128
	declare function u32_divq(byval as ulong, byref as __float128) as __float128
	declare function powq_i32(byref as __float128, byval as long) as __float128
	declare function powq_u32(byref as __float128, byval as ulong) as __float128

	declare function cnegq(byref as __complex128) as __complex128
	declare function caddq(byref as __complex128, byref as __complex128) as __complex128
	declare function caddq_i32(byref as __complex128, byval as long) as __complex128
	declare function caddq_u32(byref as __complex128, byval as ulong) as __complex128
	declare function csubq(byref as __complex128, byref as __complex128) as __complex128
	declare function csubq_i32(byref as __complex128, byval as long) as __complex128
	declare function csubq_u32(byref as __complex128, byval as ulong) as __complex128
	declare function cmulq(byref as __complex128, byref as __complex128) as __complex128
	declare function cmulq_i32(byref as __complex128, byval as long) as __complex128
	declare function cmulq_u32(byref as __complex128, byval as ulong) as __complex128
	declare function cdivq(byref as __complex128, byref as __complex128) as __complex128
	declare function cdivq_i32(byref as __complex128, byval as long) as __complex128
	declare function cdivq_u32(byref as __complex128, byval as ulong) as __complex128

	declare function i32_caddq(byval as long, byref as __complex128) as __complex128
	declare function u32_caddq(byval as ulong, byref as __complex128) as __complex128
	declare function i32_csubq(byval as long, byref as __complex128) as __complex128
	declare function u32_csubq(byval as ulong, byref as __complex128) as __complex128
	declare function i32_cmulq(byval as long, byref as __complex128) as __complex128
	declare function u32_cmulq(byval as ulong, byref as __complex128) as __complex128
	declare function i32_cdivq(byval as long, byref as __complex128) as __complex128
	declare function u32_cdivq(byval as ulong, byref as __complex128) as __complex128
	declare function cpowq_i32(byref as __complex128, byval as long) as __complex128
	declare function cpowq_u32(byref as __complex128, byval as ulong) as __complex128

	declare function flt128_double(byref as __float128) as double
	declare function flt128_i32(byref as __float128) as long
	declare function flt128_u32(byref as __float128) as ulong
	declare function flt128_i64(byref as __float128) as longint
	declare function flt128_u64(byref as __float128) as ulongint

	declare function double_flt128(byval as double) as __float128
	declare function i32_flt128(byval as long) as __float128
	declare function u32_flt128(byval as ulong) as __float128
	declare function i64_flt128(byval as longint) as __float128
	declare function u64_flt128(byval as ulongint) as __float128

	declare function cmpq(byref as __float128, byref as __float128) as long
	declare function cmpq_double(byref as __float128, byval as double) as long
	declare function cmpq_i32(byref as __float128, byval as long) as long
	declare function cmpq_i64(byref as __float128, byval as longint) as long
	declare function cmpq_u32(byref as __float128, byval as ulong) as long
	declare function cmpq_u64(byref as __float128, byval as ulongint) as long

end extern

	dim shared as __float128 FLT128_MAX = strtoflt128("1.18973149535723176508575932662800702e4932Q", 0)
	dim shared as __float128 FLT128_MIN = strtoflt128("3.36210314311209350626267781732175260e-4932Q", 0)
	dim shared as __float128 FLT128_EPSILON = strtoflt128("1.92592994438723585305597794258492732e-34Q", 0)
	dim shared as __float128 FLT128_DENORM_MIN = strtoflt128("6.475175119438025110924438958227646552e-4966Q", 0)

	const FLT128_MANT_DIG = 113
	const FLT128_MIN_EXP = -16381
	const FLT128_MAX_EXP = 16384
	const FLT128_DIG = 33
	const FLT128_MIN_10_EXP = -4931
	const FLT128_MAX_10_EXP = 4932

	dim shared as __float128 M_Eq = strtoflt128("2.718281828459045235360287471352662498Q", 0)

	dim shared as __float128 M_LOG2Eq = strtoflt128("1.442695040888963407359924681001892137Q", 0)
	dim shared as __float128 M_LOG10Eq = strtoflt128("0.434294481903251827651128918916605082Q", 0)
	dim shared as __float128 M_LN2q = strtoflt128("0.693147180559945309417232121458176568Q", 0)
	dim shared as __float128 M_LN10q = strtoflt128("2.302585092994045684017991454684364208Q", 0)
	dim shared as __float128 M_PIq = strtoflt128("3.141592653589793238462643383279502884Q", 0)
	dim shared as __float128 M_PI_2q = strtoflt128("1.570796326794896619231321691639751442Q", 0)
	dim shared as __float128 M_PI_4q = strtoflt128("0.785398163397448309615660845819875721Q", 0)
	dim shared as __float128 M_1_PIq = strtoflt128("0.318309886183790671537767526745028724Q", 0)
	dim shared as __float128 M_2_PIq = strtoflt128("0.636619772367581343075535053490057448Q", 0)
	dim shared as __float128 M_2_SQRTPIq = strtoflt128("1.128379167095512573896158903121545172Q", 0)
	dim shared as __float128 M_SQRT2q = strtoflt128("1.414213562373095048801688724209698079Q", 0)
	dim shared as __float128 M_SQRT1_2q = strtoflt128("0.707106781186547524400844362104849039Q", 0)

	'======================================================================================================

	Function __float128.toString( ) As String
		dim as string s
		dim zs as zstring ptr=callocate(128)
		quadmath_snprintf(zs, 128, "%.34Qg", this)
		s = *zs
		deallocate(zs)
		Function = s
	End Function

	Function __float128.toLong ( ) As Long
		Function = flt128_i32(this)
	End Function

	Function __float128.toDouble ( ) As Double
		Function = flt128_double(this)
	End Function

	Function __float128.toSingle ( ) As Single
		Function = csng(flt128_double(this))
	End Function

	constructor __float128 ( )
		this.mq(0) = 0ul
		this.mq(1) = 0ul
		this.mq(2) = 0ul
		this.mq(3) = 0ul
	end constructor

	constructor __float128 ( Byref rhs As __float128 )
		this.mq(0) = rhs.mq(0)
		this.mq(1) = rhs.mq(1)
		this.mq(2) = rhs.mq(2)
		this.mq(3) = rhs.mq(3)
	end constructor

	constructor __complex128 ( )
		this.re.mq(0) = 0ul
		this.re.mq(1) = 0ul
		this.re.mq(2) = 0ul
		this.re.mq(3) = 0ul
		this.im.mq(0) = 0ul
		this.im.mq(1) = 0ul
		this.im.mq(2) = 0ul
		this.im.mq(3) = 0ul
	end constructor

	constructor __complex128 ( Byref rhs As __complex128 )
		this.re.mq(0) = rhs.re.mq(0)
		this.re.mq(1) = rhs.re.mq(1)
		this.re.mq(2) = rhs.re.mq(2)
		this.re.mq(3) = rhs.re.mq(3)
		this.im.mq(0) = rhs.im.mq(0)
		this.im.mq(1) = rhs.im.mq(1)
		this.im.mq(2) = rhs.im.mq(2)
		this.im.mq(3) = rhs.im.mq(3)
	end constructor

	destructor __complex128 ( )
	end destructor

	function cfloat128(byval a as Ulong, byval b as Ulong, byval c as Ulong, byval d as Ulong) as __float128
		dim as __float128 x
		x.mq(0)=a
		x.mq(1)=b
		x.mq(2)=c
		x.mq(3)=d
		return x
	end function

	Constructor __float128 ( Byval rhs As Long )
		this = i32_flt128(rhs)
	End Constructor

	Constructor __float128 ( Byref rhs As String )
		this = strtoflt128(rhs, 0)
	End Constructor

	Constructor __float128 ( Byval rhs As Longint )
		this = i64_flt128(rhs)
	End Constructor

	Constructor __float128 ( Byval rhs As Integer )
		#if sizeof(Integer)=8
			this = i64_flt128(rhs)
		#else
			this = i32_flt128(rhs)
		#endif
	End Constructor

	Constructor __float128 ( Byval rhs As Double )
		this = double_flt128(rhs)
	End Constructor

	Constructor __float128 ( Byval rhs As Single )
		this = double_flt128(cdbl(rhs))
	End Constructor

	destructor __float128 ( )
	end destructor

	Operator __float128.let ( Byref rhs As __float128 )
		this.mq(0) = rhs.mq(0)
		this.mq(1) = rhs.mq(1)
		this.mq(2) = rhs.mq(2)
		this.mq(3) = rhs.mq(3)
	End Operator

	Operator __float128.let ( Byval rhs As Long )
		this = i32_flt128(rhs)
	End Operator

	Operator __float128.let ( Byref rhs As String )
		this = strtoflt128(rhs, 0)
	End Operator

	Operator __float128.Let ( Byval rhs As Longint )
		this = i64_flt128(rhs)
	End Operator

	Operator __float128.Let ( Byval rhs As Integer )
		#if sizeof(Integer)=8
			this = i64_flt128(rhs)
		#else
			this = i32_flt128(rhs)
		#endif
	End Operator

	Operator __float128.Let ( Byval rhs As Double )
		this = double_flt128(rhs)
	End Operator

	Operator __float128.Let ( Byval rhs As Single )
		this = double_flt128(cdbl(rhs))
	End Operator

	Operator __float128.cast ( ) As String
		dim zs as zstring ptr=callocate(128)
		quadmath_snprintf(zs, 128, "%.34Qg", this)
		Operator = *zs
		deallocate(zs)
	End Operator

	Operator __float128.cast ( ) As Long
		Operator = flt128_i32(this)
	End Operator

	Operator __float128.cast ( ) As Double
		Operator = flt128_double(this)
	End Operator

	Operator __float128.cast ( ) As Single
		Operator = flt128_double(this)
	End Operator

	Operator = ( Byref lhs As __float128, Byref rhs As __float128 ) As Long
		Operator = (cmpq(lhs, rhs) = 0)
	End Operator

	Operator < ( Byref lhs As __float128, Byref rhs As __float128 ) As Long
		Operator = (cmpq(lhs, rhs) < 0)
	End Operator

	Operator > ( Byref lhs As __float128, Byref rhs As __float128 ) As Long
		Operator = (cmpq(lhs, rhs) > 0)
	End Operator

	Operator <= ( Byref lhs As __float128, Byref rhs As __float128 ) As Long
		Operator = (cmpq(lhs, rhs) <= 0)
	End Operator

	Operator >= ( Byref lhs As __float128, Byref rhs As __float128 ) As Long
		Operator = (cmpq(lhs, rhs) >= 0)
	End Operator

	Operator <> ( Byref lhs As __float128, Byref rhs As __float128 ) As Long
		Operator = (cmpq(lhs, rhs) <> 0)
	End Operator

	Operator = ( Byref lhs As __float128, Byval rhs As long ) As Long
		Operator = (cmpq_i32(lhs, rhs) = 0)
	End Operator

	Operator < ( Byref lhs As __float128, Byval rhs As long ) As Long
		Operator = (cmpq_i32(lhs, rhs) < 0)
	End Operator

	Operator > ( Byref lhs As __float128, Byval rhs As long ) As Long
		Operator = (cmpq_i32(lhs, rhs) > 0)
	End Operator

	Operator <= ( Byref lhs As __float128, Byval rhs As long ) As Long
		Operator = (cmpq_i32(lhs, rhs) <= 0)
	End Operator

	Operator >= ( Byref lhs As __float128, Byval rhs As long ) As Long
		Operator = (cmpq_i32(lhs, rhs) >= 0)
	End Operator

	Operator <> ( Byref lhs As __float128, Byval rhs As long ) As Long
		Operator = (cmpq_i32(lhs, rhs) <> 0)
	End Operator

	Operator = ( Byref lhs As __float128, Byval rhs As ulong ) As Long
		Operator = (cmpq_u32(lhs, rhs) = 0)
	End Operator

	Operator < ( Byref lhs As __float128, Byval rhs As ulong ) As Long
		Operator = (cmpq_u32(lhs, rhs) < 0)
	End Operator

	Operator > ( Byref lhs As __float128, Byval rhs As ulong ) As Long
		Operator = (cmpq_u32(lhs, rhs) > 0)
	End Operator

	Operator <= ( Byref lhs As __float128, Byval rhs As ulong ) As Long
		Operator = (cmpq_u32(lhs, rhs) <= 0)
	End Operator

	Operator >= ( Byref lhs As __float128, Byval rhs As ulong ) As Long
		Operator = (cmpq_u32(lhs, rhs) >= 0)
	End Operator

	Operator <> ( Byref lhs As __float128, Byval rhs As ulong ) As Long
		Operator = (cmpq_u32(lhs, rhs) <> 0)
	End Operator

	Operator = ( Byref lhs As __float128, Byval rhs As longint ) As Long
		Operator = (cmpq_i64(lhs, rhs) = 0)
	End Operator

	Operator < ( Byref lhs As __float128, Byval rhs As longint ) As Long
		Operator = (cmpq_i64(lhs, rhs) < 0)
	End Operator

	Operator > ( Byref lhs As __float128, Byval rhs As longint ) As Long
		Operator = (cmpq_i64(lhs, rhs) > 0)
	End Operator

	Operator <= ( Byref lhs As __float128, Byval rhs As longint ) As Long
		Operator = (cmpq_i64(lhs, rhs) <= 0)
	End Operator

	Operator >= ( Byref lhs As __float128, Byval rhs As longint ) As Long
		Operator = (cmpq_i64(lhs, rhs) >= 0)
	End Operator

	Operator <> ( Byref lhs As __float128, Byval rhs As longint ) As Long
		Operator = (cmpq_i64(lhs, rhs) <> 0)
	End Operator

	Operator = ( Byref lhs As __float128, Byval rhs As ulongint ) As Long
		Operator = (cmpq_u64(lhs, rhs) = 0)
	End Operator

	Operator < ( Byref lhs As __float128, Byval rhs As ulongint ) As Long
		Operator = (cmpq_u64(lhs, rhs) < 0)
	End Operator

	Operator > ( Byref lhs As __float128, Byval rhs As ulongint ) As Long
		Operator = (cmpq_u64(lhs, rhs) > 0)
	End Operator

	Operator <= ( Byref lhs As __float128, Byval rhs As ulongint ) As Long
		Operator = (cmpq_u64(lhs, rhs) <= 0)
	End Operator

	Operator >= ( Byref lhs As __float128, Byval rhs As ulongint ) As Long
		Operator = (cmpq_u64(lhs, rhs) >= 0)
	End Operator

	Operator <> ( Byref lhs As __float128, Byval rhs As ulongint ) As Long
		Operator = (cmpq_u64(lhs, rhs) <> 0)
	End Operator

	Operator = ( Byref lhs As __float128, Byval rhs As Double ) As Long
		Operator = (cmpq_double(lhs, rhs) = 0)
	End Operator

	Operator < ( Byref lhs As __float128, Byval rhs As Double ) As Long
		Operator = (cmpq_double(lhs, rhs) < 0)
	End Operator

	Operator > ( Byref lhs As __float128, Byval rhs As Double ) As Long
		Operator = (cmpq_double(lhs, rhs) > 0)
	End Operator

	Operator <= ( Byref lhs As __float128, Byval rhs As Double ) As Long
		Operator = (cmpq_double(lhs, rhs) <= 0)
	End Operator

	Operator >= ( Byref lhs As __float128, Byval rhs As Double ) As Long
		Operator = (cmpq_double(lhs, rhs) >= 0)
	End Operator

	Operator <> ( Byref lhs As __float128, Byval rhs As Double ) As Long
		Operator = (cmpq_double(lhs, rhs) <> 0)
	End Operator
	
	Operator + ( Byref lhs As __float128, Byref rhs As __float128 ) As __float128
		Operator = addq(lhs, rhs)
	End Operator

	Operator + ( Byref lhs As __float128, Byval rhs As Double ) As __float128
		Operator = addq_double(lhs, rhs)
	End Operator

	Operator + ( Byval lhs As Double, Byref rhs As __float128 ) As __float128
		Operator = double_addq(lhs, rhs)
	End Operator

	Operator + ( Byref lhs As __float128, Byval rhs As Longint ) As __float128
		Operator = addq_i64(lhs, rhs)
	End Operator

	Operator + ( Byval lhs As Longint, byref rhs As __float128  ) As __float128
		Operator = i64_addq(lhs, rhs)
	End Operator

	Operator + (Byref lhs As __float128, byref rhs as String) As __float128
		dim as __float128 result
		result=strtoflt128(rhs, 0)
		Operator = addq(lhs, result)
	End Operator

	Operator - ( Byref lhs As __float128, Byref rhs As __float128 ) As __float128
		Operator = subq(lhs, rhs)
	End Operator

	Operator - ( Byref lhs As __float128, Byval rhs As Double ) As __float128
		Operator = subq_double(lhs, rhs)
	End Operator

	Operator - ( Byval lhs As Double, Byref rhs As __float128 ) As __float128
		Operator = double_subq(lhs, rhs)
	End Operator

	Operator - ( Byref lhs As __float128, Byval rhs As Longint ) As __float128
		Operator = subq_i64(lhs, rhs)
	End Operator

	Operator - ( Byval lhs As Longint, byref rhs As __float128  ) As __float128
		Operator = i64_subq(lhs, rhs)
	End Operator

	Operator - (Byref lhs As __float128, byref rhs as String) As __float128
		dim as __float128 result
		result=strtoflt128(rhs, 0)
		Operator = subq(lhs, result)
	End Operator

	Operator * ( Byref lhs As __float128, Byref rhs As __float128 ) As __float128
		Operator = mulq(lhs, rhs)
	End Operator

	Operator * ( Byref lhs As __float128, Byval rhs As Double ) As __float128
		Operator = mulq_double(lhs, rhs)
	End Operator

	Operator * ( Byval lhs As Double, Byref rhs As __float128 ) As __float128
		Operator = double_mulq(lhs, rhs)
	End Operator

	Operator * ( Byref lhs As __float128, Byval rhs As Longint ) As __float128
		Operator = mulq_i64(lhs, rhs)
	End Operator

	Operator * ( Byval lhs As Longint, byref rhs As __float128  ) As __float128
		Operator = i64_mulq(lhs, rhs)
	End Operator

	Operator * (Byref lhs As __float128, byref rhs as String) As __float128
		dim as __float128 result
		result=strtoflt128(rhs, 0)
		Operator = mulq(lhs, result)
	End Operator

	Operator / ( Byref lhs As __float128, Byref rhs As __float128 ) As __float128
		Operator = divq(lhs, rhs)
	End Operator

	Operator / ( Byref lhs As __float128, Byval rhs As Double ) As __float128
		Operator = divq_double(lhs, rhs)
	End Operator

	Operator / ( Byval lhs As Double, Byref rhs As __float128 ) As __float128
		Operator = double_divq(lhs, rhs)
	End Operator

	Operator / ( Byref lhs As __float128, Byval rhs As Longint ) As __float128
		Operator = divq_i64(lhs, rhs)
	End Operator

	Operator / ( Byval lhs As Longint, byref rhs As __float128  ) As __float128
		Operator = i64_divq(lhs, rhs)
	End Operator

	Operator / (Byref lhs As __float128, byref rhs as String) As __float128
		dim as __float128 result
		result=strtoflt128(rhs, 0)
		Operator = divq(lhs, result)
	End Operator

	Operator ^ ( Byref lhs As __float128, Byref rhs As __float128 ) As __float128
		Operator = powq(lhs, rhs)
	End Operator
	
	Operator ^ ( Byref lhs As __float128, Byval rhs As long ) As __float128
		Operator = powq_i32(lhs, rhs)
	End Operator
	
	Operator ^ ( Byref lhs As __float128, Byval rhs As ulong ) As __float128
		Operator = powq_u32(lhs, rhs)
	End Operator

	Operator __float128.+= (Byref rhs As __float128)
		this = addq(this, rhs)
	End Operator

	Operator __float128.+= (Byval rhs As Longint)
		this = addq_i64(this, rhs)
	End Operator

	Operator __float128.+= (Byval rhs As Double)
		this = addq_double(this, rhs)
	End Operator

	Operator __float128.+= (Byval rhs As Single)
		this = addq_double(this, cdbl(rhs))
	End Operator

	Operator __float128.+= (Byref rhs As String)
		Dim As __float128 result
		result=strtoflt128(rhs, 0)
		this = addq(this, result)
	End Operator

	Operator __float128.-= (Byref rhs As __float128)
		this = subq(this, rhs)
	End Operator

	Operator __float128.-= (Byval rhs As Longint)
		this = subq_i64(this, rhs)
	End Operator

	Operator __float128.-= (Byval rhs As Double)
		this = subq_double(this, rhs)
	End Operator

	Operator __float128.-= (Byval rhs As Single)
		this = subq_double(this, cdbl(rhs))
	End Operator

	Operator __float128.-= (Byref rhs As String)
		Dim As __float128 result
		result=strtoflt128(rhs, 0)
		this = subq(this, result)
	End Operator

	Operator __float128.*= (Byref rhs As __float128)
		this = mulq(this, rhs)
	End Operator

	Operator __float128.*= (Byval rhs As Longint)
		this = mulq_i64(this, rhs)
	End Operator

	Operator __float128.*= (Byval rhs As Double)
		this = mulq_double(this, rhs)
	End Operator

	Operator __float128.*= (Byval rhs As Single)
		this = mulq_double(this, cdbl(rhs))
	End Operator

	Operator __float128.*= (Byref rhs As String)
		Dim As __float128 result
		result=strtoflt128(rhs, 0)
		this = mulq(this, result)
	End Operator

	Operator __float128./= (Byref rhs As __float128)
		this = divq(this, rhs)
	End Operator

	Operator __float128./= (Byval rhs As Longint)
		this = divq_i64(this, rhs)
	End Operator

	Operator __float128./= (Byval rhs As Double)
		this = divq_double(this, rhs)
	End Operator

	Operator __float128./= (Byval rhs As Single)
		this = divq_double(this, cdbl(rhs))
	End Operator

	Operator __float128./= (Byref rhs As String)
		Dim As __float128 result
		result=strtoflt128(rhs, 0)
		this = divq(this, result)
	End Operator

	Operator __float128.^= ( Byref rhs As __float128 )
		this = powq(this, rhs)
	End Operator
	
	Operator __float128.^= ( Byval rhs As long )
		this = powq_i32(this, rhs)
	End Operator
	
	Operator __float128.^= ( Byval rhs As ulong )
		this = powq_u32(this, rhs)
	End Operator

	'============================================================================
	'' For Next for __float128 type
	''
	'' implicit step versions
	'' 
	'' In this example, we interpret implicit step
	'' to mean 1
	Operator __float128.for ( )
	End Operator
	 
	Operator __float128.step ( )
			This += 1 'this = this+1 '
	End Operator 
	 
	Operator __float128.next ( Byref end_cond As __float128 ) As Integer
			Return This <= end_cond
	End Operator
	 
	 
	'' explicit step versions
	'' 
	Operator __float128.for ( Byref step_var As __float128 )
	End Operator
	 
	Operator __float128.step ( Byref step_var As __float128 )
			This += step_var 'this = this + step_var '    
	End Operator 
	 
	Operator __float128.next ( Byref end_cond As __float128, Byref step_var As __float128 ) As Integer
			If step_var < 0 Then
					Return This >= end_cond
			Else
					Return This <= end_cond
			End If
	End Operator	

'==================================================================================================================
