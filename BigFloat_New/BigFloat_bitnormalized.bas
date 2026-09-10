
' ============================================================
' BigFloat.bas - binary floating-point engine for FreeBASIC
' Canonical finite nonzero invariant: bit 31 of aSig(0) is set.
' value = (-1)^sign * significand * 2^exponent
' Round mode: nearest, ties to even.
' ============================================================

Type mylong As Long

' 13 limbs x 32 bits = 416 bits.
Const SIG_LEN   As mylong = 14
Const SIG_BITS  As mylong = 32 * SIG_LEN
Const SIG_BYTES As mylong = 4 * SIG_LEN   ' byte helpers / decimal conversion only

dim shared as long SIN_TAYLOR_N

Type BigFloat
    sign As Long        ' 0 = non-negative, 1 = negative
    bIsNaN As Long      ' 0 = normal value, 1 = NaN
    exponent As Long    ' units of BITS (base 2); value = significand * 2^exponent
    aSig(0 To SIG_LEN - 1) As ULong
End Type

Type BigFloat_t
	Declare Constructor ( )
	Declare Constructor ( Byval rhs As Long )
	Declare Constructor ( Byval rhs As Longint )
	Declare Constructor ( Byval rhs As uLong )
	Declare Constructor ( Byval rhs As uLongint )
	Declare Constructor ( Byval rhs As Double )
	Declare Constructor ( Byref rhs As String )
	Declare Constructor ( Byref rhs As BigFloat_t )

	Declare Destructor ( )

	Declare Operator Let ( Byval rhs As Long )
	Declare Operator Let ( Byval rhs As Longint )
	Declare Operator Let ( Byval rhs As ULong )
	Declare Operator Let ( Byval rhs As ULongint )
	Declare Operator Let ( Byval rhs As Double )
	Declare Operator Let ( Byref rhs As String )
	Declare Operator Let ( Byref rhs As BigFloat_t )

	Declare Operator Cast ( ) As Long
	Declare Operator Cast ( ) As LongInt
	Declare Operator Cast ( ) As Double
	Declare Operator Cast ( ) As String

	BigNum as BigFloat
End Type

Declare sub init_constants
dim shared as BigFloat_t fppi, fppi2, fpPihalf, Pi3o2, fpln2

init_constants
' ==============================================================
' Forward declarations
' ==============================================================
Declare Sub      BigFloat_Add       (ByRef Result As BigFloat, ByRef A As BigFloat, ByRef B As BigFloat)
Declare Sub      BigFloat_Subtract  (ByRef Result As BigFloat, ByRef A As BigFloat, ByRef B As BigFloat)
Declare Sub      BigFloat_Multiply  (ByRef Result As BigFloat, ByRef A As BigFloat, ByRef B As BigFloat)
Declare Sub      BigFloat_Divide    (ByRef Result As BigFloat, ByRef A As BigFloat, ByRef B As BigFloat)
Declare Sub      BigFloat_FromString(ByRef Result As BigFloat, ByVal sInput As String)
Declare Function BigFloat_Compare   (ByRef A As BigFloat, ByRef B As BigFloat) As mylong
Declare Sub      BigFloat_Clear     (ByRef F As BigFloat)
Declare Sub      BigFloat_Assign    (ByRef Dst As BigFloat, ByRef Src As BigFloat)
Declare Sub      BigFloat_SetNaN    (ByRef F As BigFloat)
Declare Function BigFloat_ToString  (ByRef F As BigFloat) As String
Declare Function IsExpNeg       (ByRef F As BigFloat) As Boolean

' Private helpers
Declare Function GetExp   (ByRef F As BigFloat) As Long
Declare Sub      SetExp   (ByRef F As BigFloat, ByVal lE As Long)
Declare Function IsNaN_F  (ByRef F As BigFloat) As Boolean
Declare Function IsNeg_F  (ByRef F As BigFloat) As Boolean
Declare Function IsZero_F (ByRef F As BigFloat) As Boolean
Declare Sub      Normalize (ByRef F As BigFloat)
Declare Function SigGetByte(ByRef F As BigFloat, ByVal iByte As Long) As UByte
Declare Sub      SigSetByte(ByRef F As BigFloat, ByVal iByte As Long, ByVal v As UByte)
Declare Function BigFloat_GetSigBit(ByRef X As BigFloat, _
                                ByVal bitIndex As Long) As ULong
                                
' Native base-2^32 limb helpers
Declare Sub      SigToLE(ByRef F As BigFloat, A() As ULong)
Declare Sub      SigFromLE(ByRef F As BigFloat, A() As ULong)
Declare Function SignificantBytesLE(A() As ULong) As Long
Declare Function GetByteLE(A() As ULong, ByVal byteIndex As Long) As UByte
Declare Function AnyLowBytesNonZeroLE(A() As ULong, ByVal byteCount As Long) As Boolean
Declare Sub      ShiftLeftBytesLE(Src() As ULong, Dst() As ULong, ByVal shiftBytes As Long)
Declare Sub      ShiftRightBytesTo7LE(Src() As ULong, Dst() As ULong, ByVal shiftBytes As Long)
Declare Sub      RoundExactLimbs(ByRef Result As BigFloat, A() As ULong, ByVal lBaseExp As Long, ByVal blExternalSticky As Boolean)
Declare Function LeadingZeroBits32(ByVal x As ULong) As Long
Declare Sub      Div224x224(ByRef A As BigFloat, ByRef B As BigFloat, Q() As ULong, R() As ULong)
Declare Function CompareTwiceRemainder32(R() As ULong, D() As ULong) As Long

Declare Sub LimbMulAccumulate(X() As ULong, ByVal nx As Long, Y() As ULong, ByVal ny As Long, P() As ULong)

Declare Sub SigAdd(ByRef Result As BigFloat, ByRef A As BigFloat, ByRef B As BigFloat, _
                   ByVal lExpA As Long, ByVal lExpB As Long)
Declare Sub SigSub(ByRef Result As BigFloat, ByRef A As BigFloat, ByRef B As BigFloat, _
                   ByVal lExpA As Long, ByVal lExpB As Long)
Declare Sub RoundExactBytes(ByRef Result As BigFloat, A() As Long, ByVal lBaseExp As Long, _
                            ByVal blExternalSticky As Boolean)
Declare Sub BigByte_MulSmall(A() As Long, ByVal m As Long)
Declare Sub BigByte_AddSmall(A() As Long, ByVal v As Long)
Declare Function BigByte_DivSmall(A() As Long, ByVal d As Long) As Long

Declare Sub SigRoundIncrement(ByRef F As BigFloat, ByRef lExp As Long)

Declare Sub      BigDec_Init    (A() As mylong)
Declare Sub      BigDec_TrimMSZ (A() As mylong)
Declare Sub      BigDec_MulSmall(A() As mylong, ByVal m As mylong)
Declare Sub      BigDec_AddSmall(A() As mylong, ByVal addVal As mylong)
Declare Function BigDec_ToString(A() As mylong) As String

Declare Sub BigFloat_FromULong   (ByRef R As BigFloat, ByVal v As ULong)
Declare Sub BigFloat_FromLong    (ByRef R As BigFloat, ByVal v As Long)
Declare Sub BigFloat_FromULongInt(ByRef R As BigFloat, ByVal v As ULongInt)
Declare Sub BigFloat_FromLongInt (ByRef R As BigFloat, ByVal v As LongInt)
Declare Sub BigFloat_FromDouble  (ByRef R As BigFloat, ByVal v As Double)

Declare Function BigFloat_ToULong   (ByRef X As BigFloat, ByRef v As ULong) As Boolean
Declare Function BigFloat_ToLong    (ByRef X As BigFloat, ByRef v As Long) As Boolean
Declare Function BigFloat_ToULongInt(ByRef X As BigFloat, ByRef v As ULongInt) As Boolean
Declare Function BigFloat_ToLongInt (ByRef X As BigFloat, ByRef v As LongInt) As Boolean
Declare Function BigFloat_ToDouble  (ByRef X As BigFloat) As Double

Declare Sub BigFloat_Frexp(ByRef X As BigFloat, _
               ByRef Mantissa As BigFloat, _
               ByRef BinExponent As Long)
Declare Sub BigFloat_Mul2(ByRef Result As BigFloat, ByRef X As BigFloat)
Declare Sub BigFloat_Div2(ByRef Result As BigFloat, ByRef X As BigFloat)
Declare Sub BigFloat_Abs(ByRef Result As BigFloat, ByRef X As BigFloat)
Declare Sub BigFloat_Negate(ByRef Result As BigFloat, ByRef X As BigFloat)

' Arithmetic against a plain 64-bit integer scalar.  Add/Subtract/
' Divide are correctness-first wrappers built on BigFloat_FromLongInt /
' BigFloat_FromULongInt plus the existing general routines (building the
' scalar operand is O(1), so this costs almost nothing extra).
' Multiply gets a genuinely dedicated fast path -- see
' BigFloat_MultiplyU64 -- since a 224x64 product is much cheaper than a
' full 224x224 one and this is a common enough operation to be worth
' the dedicated code.
Declare Sub BigFloat_AddI64      (ByRef Result As BigFloat, ByRef X As BigFloat, ByVal v As LongInt)
Declare Sub BigFloat_AddU64      (ByRef Result As BigFloat, ByRef X As BigFloat, ByVal v As ULongInt)
Declare Sub BigFloat_SubtractI64 (ByRef Result As BigFloat, ByRef X As BigFloat, ByVal v As LongInt)
Declare Sub BigFloat_SubtractU64 (ByRef Result As BigFloat, ByRef X As BigFloat, ByVal v As ULongInt)
Declare Sub BigFloat_MultiplyI64 (ByRef Result As BigFloat, ByRef X As BigFloat, ByVal v As LongInt)
Declare Sub BigFloat_MultiplyU64 (ByRef Result As BigFloat, ByRef X As BigFloat, ByVal mag As ULongInt)
Declare Sub BigFloat_DivideI64   (ByRef Result As BigFloat, ByRef X As BigFloat, ByVal v As LongInt)
Declare Sub BigFloat_DivideU64   (ByRef Result As BigFloat, ByRef X As BigFloat, ByVal v As ULongInt)

' ==============================================================
' ADD
' Round-to-nearest, ties-to-even.
' ==============================================================
Sub BigFloat_Add(ByRef Result As BigFloat, ByRef A As BigFloat, ByRef B As BigFloat)
    If IsNaN_F(A) OrElse IsNaN_F(B) Then
        BigFloat_SetNaN Result
        Exit Sub
    End If

    ' Zero has no meaningful exponent.  Handle it before any
    ' exponent comparison/alignment.
    If IsZero_F(A) Then
        BigFloat_Assign Result, B
        Exit Sub
    End If

    If IsZero_F(B) Then
        BigFloat_Assign Result, A
        Exit Sub
    End If

    ' Local copies make Result=A+B safe even when Result aliases A or B.
    Dim AA As BigFloat, BB As BigFloat
    BigFloat_Assign AA, A
    BigFloat_Assign BB, B

    Dim lExpA As Long = GetExp(AA)
    Dim lExpB As Long = GetExp(BB)
    Dim blNegA As Boolean = IsNeg_F(AA)
    Dim blNegB As Boolean = IsNeg_F(BB)

    If blNegA = blNegB Then
        If lExpA >= lExpB Then
            SigAdd Result, AA, BB, lExpA, lExpB
        Else
            SigAdd Result, BB, AA, lExpB, lExpA
        End If

        If blNegA AndAlso Not IsZero_F(Result) Then
            Result.sign = 1
        End If

    Else
        Dim blABigger As Boolean
        Dim iSigIdx As mylong

        If lExpA > lExpB Then
            blABigger = True
        ElseIf lExpA < lExpB Then
            blABigger = False
        Else
            blABigger = True
            For iSigIdx = 0 To SIG_LEN - 1
                If AA.aSig(iSigIdx) > BB.aSig(iSigIdx) Then
                    blABigger = True
                    Exit For
                ElseIf AA.aSig(iSigIdx) < BB.aSig(iSigIdx) Then
                    blABigger = False
                    Exit For
                End If
            Next
        End If

        If blABigger Then
            SigSub Result, AA, BB, lExpA, lExpB
            If blNegA AndAlso Not IsZero_F(Result) Then Result.sign = 1
        Else
            SigSub Result, BB, AA, lExpB, lExpA
            If blNegB AndAlso Not IsZero_F(Result) Then Result.sign = 1
        End If
    End If
End Sub

' ==============================================================
' SUBTRACT
' ==============================================================
Sub BigFloat_Subtract(ByRef Result As BigFloat, ByRef A As BigFloat, ByRef B As BigFloat)
    If IsNaN_F(A) OrElse IsNaN_F(B) Then
        BigFloat_SetNaN Result
        Exit Sub
    End If

    Dim Bx As BigFloat
    BigFloat_Assign Bx, B

    Bx.sign Xor= 1

    BigFloat_Add Result, A, Bx
End Sub

' ==============================================================
' MULTIPLY
' Native 7 x 7 base-2^32 product followed by one RNE rounding.
' ==============================================================
Sub BigFloat_Multiply(ByRef Result As BigFloat, ByRef A As BigFloat, ByRef B As BigFloat)
    If IsNaN_F(A) OrElse IsNaN_F(B) Then
        BigFloat_SetNaN Result
        Exit Sub
    End If

    If IsZero_F(A) OrElse IsZero_F(B) Then
        BigFloat_Clear Result
        Exit Sub
    End If

    Dim blNegResult As Boolean
    blNegResult = (IsNeg_F(A) Xor IsNeg_F(B))

    ' Fixed-size stack buffers: sizes are compile-time constants, so
    ' there is no reason to pay for a heap ReDim on every multiply.
    Dim AL(0 To SIG_LEN - 1) As ULong
    Dim BL(0 To SIG_LEN - 1) As ULong
    Dim i As Long

    For i = 0 To SIG_LEN - 1
        AL(i) = A.aSig(SIG_LEN - 1 - i)
        BL(i) = B.aSig(SIG_LEN - 1 - i)
    Next

    ' Exact 224 x 224 -> 448 bit product in base 2^32.
    Dim P(0 To 2 * SIG_LEN) As ULong

    Dim j As Long, k As Long
    Dim carry As ULongInt
    Dim t As ULongInt

    For i = 0 To SIG_LEN - 1
        carry = 0

        For j = 0 To SIG_LEN - 1
            k = i + j

            ' This sum is <= 2^64-1:
            '   existing limb + 32x32 product + carry.
            t = CULngInt(P(k)) _
              + CULngInt(AL(i)) * CULngInt(BL(j)) _
              + carry

            P(k) = CULng(t And &HFFFFFFFFULL)
            carry = t Shr 32
        Next

        k = i + SIG_LEN

        Do While carry <> 0
            t = CULngInt(P(k)) + carry
            P(k) = CULng(t And &HFFFFFFFFULL)
            carry = t Shr 32
            k += 1
        Loop
    Next

    ' P is the exact integer MA*MB and represents
    ' P * 2^(EA+EB).  Round once, directly from 32-bit limbs.
    RoundExactLimbs Result, P(), GetExp(A) + GetExp(B), False

    If blNegResult AndAlso Not IsZero_F(Result) Then
        Result.sign = 1
    End If
End Sub

' ==============================================================
' Generic little-endian schoolbook accumulate:
'
'       P(0 To nx+ny-1) += X(0 To nx-1) * Y(0 To ny-1)
'
' P is ADDED into (not cleared), so it can be reused to merge
' several partial products, and callers must zero it first if
' they want a clean product.  P must have room for a carry to
' ripple one limb past index nx+ny-1 in the rare defensive case.
' ==============================================================
Sub LimbMulAccumulate(X() As ULong, ByVal nx As Long, Y() As ULong, ByVal ny As Long, P() As ULong)
    Dim i As Long, j As Long, k As Long
    Dim carry As ULongInt, t As ULongInt

    For i = 0 To nx - 1
        carry = 0

        For j = 0 To ny - 1
            k = i + j
            t = CULngInt(P(k)) + CULngInt(X(i)) * CULngInt(Y(j)) + carry
            P(k) = CULng(t And &HFFFFFFFFULL)
            carry = t Shr 32
        Next

        k = i + ny
        Do While carry <> 0
            t = CULngInt(P(k)) + carry
            P(k) = CULng(t And &HFFFFFFFFULL)
            carry = t Shr 32
            k += 1
        Loop
    Next
End Sub

' ==============================================================
' DIVIDE
'
' Representation:
'
'       value = M * 2^E
'
' where M is the unsigned 224-bit integer stored in aSig().
'
' We form
'
'       Q = floor((MA * 256^28) / MB)
'
' Q may require 29 bytes.
'
' Rounding mode:
'       round to nearest, ties to even
' ==============================================================
Sub BigFloat_Divide(ByRef Result As BigFloat, ByRef A As BigFloat, ByRef B As BigFloat)

    If IsNaN_F(A) OrElse IsNaN_F(B) OrElse IsZero_F(B) Then
        BigFloat_SetNaN Result
        Exit Sub
    End If

    If IsZero_F(A) Then
        BigFloat_Clear Result
        Exit Sub
    End If

    Dim AA As BigFloat, BB As BigFloat
    BigFloat_Assign AA, A
    BigFloat_Assign BB, B
    AA.sign = 0
    BB.sign = 0

    Dim blNegResult As Boolean
    blNegResult = (IsNeg_F(A) Xor IsNeg_F(B))

    ' Compute exactly:
    '
    '       Q,R = (MA * 2^224) div MB
    '
    ' Since 2^224 = 256^28,
    '
    '       A/B = (Q + R/MB) * 256^(EA-EB-28)
    Dim Q(0 To SIG_LEN) As ULong        ' 8 quotient limbs (fixed, no heap alloc)
    Dim R(0 To SIG_LEN - 1) As ULong    ' 7 remainder limbs
    Div224x224 AA, BB, Q(), R()

    Dim lNewExp As Long
    lNewExp = GetExp(A) - GetExp(B) - SIG_BITS

    Dim D(0 To SIG_LEN - 1) As ULong
    Dim iD As Long
    For iD = 0 To SIG_LEN - 1
        D(iD) = BB.aSig(SIG_LEN - 1 - iD)
    Next

    Dim qBytes As Long
    qBytes = SignificantBytesLE(Q())

    Dim remNonZero As Boolean = False
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        If R(i) <> 0 Then
            remNonZero = True
            Exit For
        End If
    Next

    If qBytes > SIG_BYTES Then
        ' Q is a 29-byte integer.  RoundExactLimbs drops the one
        ' low quotient byte.  A nonzero division remainder is
        ' sticky information below that byte.
        RoundExactLimbs Result, Q(), lNewExp, remNonZero

    Else
        ' For normalized 224-bit operands Q is always at least
        ' 28 bytes.  Here it therefore fits exactly in 7 limbs.
        BigFloat_Clear Result

        For i = 0 To SIG_LEN - 1
            Result.aSig(SIG_LEN - 1 - i) = Q(i)
        Next

        ' No quotient byte was discarded.  Decide rounding from
        ' the exact remainder: compare 2*R with MB.
        Dim cmpHalf As Long
        cmpHalf = CompareTwiceRemainder32(R(), D())

        Dim roundUp As Boolean = False

        If cmpHalf > 0 Then
            roundUp = True
        ElseIf cmpHalf = 0 Then
            If (Result.aSig(SIG_LEN - 1) And 1UL) <> 0 Then
                roundUp = True
            End If
        End If

        If roundUp Then SigRoundIncrement Result, lNewExp
        SetExp Result, lNewExp
    End If

    If blNegResult AndAlso Not IsZero_F(Result) Then
        Result.sign = 1
    End If
End Sub

' ==============================================================
' Count leading zero bits in a nonzero 32-bit limb.
' ==============================================================
Function LeadingZeroBits32(ByVal x As ULong) As Long
    If x = 0 Then Return 32

    Dim n As Long = 0

    If (x And &HFFFF0000UL) = 0 Then
        n += 16
        x Shl= 16
    End If

    If (x And &HFF000000UL) = 0 Then
        n += 8
        x Shl= 8
    End If

    If (x And &HF0000000UL) = 0 Then
        n += 4
        x Shl= 4
    End If

    If (x And &HC0000000UL) = 0 Then
        n += 2
        x Shl= 2
    End If

    If (x And &H80000000UL) = 0 Then n += 1

    Return n
End Function

' ==============================================================
' Div224x224
'
' Native base-2^32 Knuth-style long division.
'
' Dividend is MA * (2^32)^7, i.e. fourteen significant limbs
' plus one normalization limb.  Divisor has seven limbs.
' The quotient therefore has at most eight limbs.
'
' Arrays Q() and R() are little-endian.
' ==============================================================
Sub Div224x224(ByRef A As BigFloat, ByRef B As BigFloat, _
               Q() As ULong, R() As ULong)

    Const BASE64 As ULongInt = 4294967296ULL
    Const MASK64 As ULongInt = &HFFFFFFFFULL

    Dim aL(0 To SIG_LEN - 1) As ULong
    Dim v(0 To SIG_LEN - 1) As ULong
    Dim i0 As Long
    For i0 = 0 To SIG_LEN - 1
        aL(i0) = A.aSig(SIG_LEN - 1 - i0)
        v(i0)  = B.aSig(SIG_LEN - 1 - i0)
    Next

    ' Q() and R() are now expected to already be sized by the caller
    ' (BigFloat_Divide declares them as fixed arrays) -- no ReDim here.

    Dim s As Long
    s = LeadingZeroBits32(v(SIG_LEN - 1))

    Dim vn(0 To SIG_LEN - 1) As ULong
    Dim un(0 To 2 * SIG_LEN) As ULong

    Dim i As Long
    Dim t As ULongInt
    Dim carry As ULongInt

    ' Normalize divisor so bit 31 of its top limb is set.
    carry = 0
    If s = 0 Then
        For i = 0 To SIG_LEN - 1
            vn(i) = v(i)
        Next
    Else
        For i = 0 To SIG_LEN - 1
            t = (CULngInt(v(i)) Shl s) Or carry
            vn(i) = CULng(t And MASK64)
            carry = t Shr 32
        Next
    End If

    ' Dividend before normalization is:
    '
    '   [7 zero low limbs][7 limbs of MA]
    '
    ' in little-endian order.
    Dim rawU(0 To 2 * SIG_LEN - 1) As ULong
    For i = 0 To SIG_LEN - 1
        rawU(i) = 0
        rawU(i + SIG_LEN) = aL(i)
    Next

    carry = 0
    If s = 0 Then
        For i = 0 To 2 * SIG_LEN - 1
            un(i) = rawU(i)
        Next
        un(2 * SIG_LEN) = 0
    Else
        For i = 0 To 2 * SIG_LEN - 1
            t = (CULngInt(rawU(i)) Shl s) Or carry
            un(i) = CULng(t And MASK64)
            carry = t Shr 32
        Next
        un(2 * SIG_LEN) = CULng(carry)
    End If

    Dim j As Long
    Dim qhat As ULongInt
    Dim rhat As ULongInt
    Dim num As ULongInt
    Dim lhs As ULongInt
    Dim rhs As ULongInt
    Dim p As ULongInt
    Dim pLow As ULongInt
    Dim borrow As ULongInt
    Dim ui As ULongInt
    Dim negative As Boolean
    Dim sum As ULongInt

    ' n=7, m=7 in Knuth Algorithm D notation.
    For j = SIG_LEN To 0 Step -1

        ' Estimate quotient limb from the top two dividend limbs.
        If un(j + SIG_LEN) = vn(SIG_LEN - 1) Then
            qhat = BASE64 - 1
            rhat = CULngInt(un(j + SIG_LEN - 1)) _
                 + CULngInt(vn(SIG_LEN - 1))
        Else
            num = (CULngInt(un(j + SIG_LEN)) Shl 32) _
                Or CULngInt(un(j + SIG_LEN - 1))

            qhat = num \ CULngInt(vn(SIG_LEN - 1))
            rhat = num Mod CULngInt(vn(SIG_LEN - 1))
        End If

        ' Correct qhat using the next divisor limb.
        Do
            If rhat >= BASE64 Then Exit Do

            lhs = qhat * CULngInt(vn(SIG_LEN - 2))
            rhs = (rhat Shl 32) Or CULngInt(un(j + SIG_LEN - 2))

            If lhs <= rhs Then Exit Do

            qhat -= 1
            rhat += CULngInt(vn(SIG_LEN - 1))
        Loop

        ' un[j..j+7] -= qhat * vn[0..6]
        borrow = 0

        For i = 0 To SIG_LEN - 1
            p = qhat * CULngInt(vn(i)) + borrow
            pLow = p And MASK64
            borrow = p Shr 32

            ui = CULngInt(un(j + i))

            If ui < pLow Then
                un(j + i) = CULng(BASE64 + ui - pLow)
                borrow += 1
            Else
                un(j + i) = CULng(ui - pLow)
            End If
        Next

        ui = CULngInt(un(j + SIG_LEN))

        If ui < borrow Then
            un(j + SIG_LEN) = CULng(BASE64 + ui - borrow)
            negative = True
        Else
            un(j + SIG_LEN) = CULng(ui - borrow)
            negative = False
        End If

        ' qhat was one too large: add divisor back once.
        If negative Then
            qhat -= 1
            carry = 0

            For i = 0 To SIG_LEN - 1
                sum = CULngInt(un(j + i)) _
                    + CULngInt(vn(i)) _
                    + carry

                un(j + i) = CULng(sum And MASK64)
                carry = sum Shr 32
            Next

            un(j + SIG_LEN) = _
                CULng((CULngInt(un(j + SIG_LEN)) + carry) And MASK64)
        End If

        Q(j) = CULng(qhat)
    Next

    ' Undo normalization of the remainder.
    If s = 0 Then
        For i = 0 To SIG_LEN - 1
            R(i) = un(i)
        Next
    Else
        For i = 0 To SIG_LEN - 1
            R(i) = (un(i) Shr s) _
                 Or (un(i + 1) Shl (32 - s))
        Next
    End If
End Sub

' ==============================================================
' Compare 2*R with D, both 7-limb little-endian integers.
' ==============================================================
Function CompareTwiceRemainder32(R() As ULong, D() As ULong) As Long
    Dim T(0 To SIG_LEN) As ULong
    Dim carry As ULongInt = 0
    Dim x As ULongInt
    Dim i As Long

    For i = 0 To SIG_LEN - 1
        x = CULngInt(R(i)) * 2ULL + carry
        T(i) = CULng(x And &HFFFFFFFFULL)
        carry = x Shr 32
    Next

    T(SIG_LEN) = CULng(carry)

    If T(SIG_LEN) <> 0 Then Return 1

    For i = SIG_LEN - 1 To 0 Step -1
        If T(i) < D(i) Then Return -1
        If T(i) > D(i) Then Return 1
    Next

    Return 0
End Function

' ==============================================================
' Add one ulp to the 224-bit significand, natively by limbs.
' ==============================================================
Sub SigRoundIncrement(ByRef F As BigFloat, ByRef lExp As Long)
    Dim i As Long
    Dim t As ULongInt
    Dim carry As ULongInt = 1

    For i = SIG_LEN - 1 To 0 Step -1
        t = CULngInt(F.aSig(i)) + carry
        F.aSig(i) = CULng(t And &HFFFFFFFFULL)
        carry = t Shr 32
        If carry = 0 Then Exit For
    Next

    If carry <> 0 Then
        For i = 0 To SIG_LEN - 1
            F.aSig(i) = 0
        Next
        F.aSig(0) = &H80000000UL
        lExp += 1
    End If
End Sub

' ==============================================================
' FROMSTRING
' Build significand directly via byte array * 10 + digit
' ==============================================================
Sub BigFloat_FromString(ByRef Result As BigFloat, ByVal sInput As String)
    BigFloat_Clear Result

    Dim s As String
    s = Trim(sInput)

    If Len(s) = 0 Then
        BigFloat_SetNaN Result
        Exit Sub
    End If

    Dim blNeg As Boolean = False

    If Left(s, 1) = "-" Then
        blNeg = True
        s = Mid(s, 2)
    ElseIf Left(s, 1) = "+" Then
        s = Mid(s, 2)
    End If

    If Len(s) = 0 Then
        BigFloat_SetNaN Result
        Exit Sub
    End If

    ' ----------------------------------------------------------
    ' Split optional decimal exponent.
    ' ----------------------------------------------------------
    Dim lDecExp As Long = 0
    Dim iEpos As Long
    iEpos = InStr(LCase(s), "e")

    If iEpos > 0 Then
        Dim sExp As String
        sExp = Mid(s, iEpos + 1)

        If Len(sExp) = 0 Then
            BigFloat_SetNaN Result
            Exit Sub
        End If

        lDecExp = CLng(sExp)
        s = Left(s, iEpos - 1)
    End If

    ' ----------------------------------------------------------
    ' Remove decimal point.  lDecExp becomes the power of ten
    ' multiplying the resulting integer digit string.
    '
    ' Example:
    '
    '     3.14159  ->  314159 * 10^-5
    ' ----------------------------------------------------------
    Dim iDot As Long
    iDot = InStr(s, ".")

    If iDot > 0 Then
        ' Reject a second decimal point.
        If InStr(iDot + 1, s, ".") > 0 Then
            BigFloat_SetNaN Result
            Exit Sub
        End If

        lDecExp -= Len(s) - iDot
        s = Left(s, iDot - 1) & Mid(s, iDot + 1)
    End If

    If Len(s) = 0 Then
        BigFloat_SetNaN Result
        Exit Sub
    End If

    ' Validate all decimal digits.
    Dim i As Long
    Dim digit As Long

    For i = 1 To Len(s)
        digit = Asc(Mid(s, i, 1)) - Asc("0")

        If digit < 0 OrElse digit > 9 Then
            BigFloat_SetNaN Result
            Exit Sub
        End If
    Next

    ' Remove leading zeroes so the precision estimate below is based
    ' on the actual number of significant decimal digits.
    Do While Len(s) > 1 AndAlso Left(s, 1) = "0"
        s = Mid(s, 2)
    Loop

    If s = "0" Then
        BigFloat_Clear Result
        Exit Sub
    End If

    ' ----------------------------------------------------------
    ' Build the decimal integer exactly in a dynamic, big-endian
    ' base-256 byte array.  Unlike the old implementation, this
    ' array is NOT limited to 28 bytes.
    ' ----------------------------------------------------------
    Dim N() As Long
    ReDim N(0 To 0)
    N(0) = 0

    For i = 1 To Len(s)
        digit = Asc(Mid(s, i, 1)) - Asc("0")
        BigByte_MulSmall N(), 10
        BigByte_AddSmall N(), digit
    Next

    If lDecExp >= 0 Then
        ' Exact multiplication by 10^lDecExp.
        For i = 1 To lDecExp
            BigByte_MulSmall N(), 10
        Next

        ' Round exactly once to the 224-bit significand.
        RoundExactBytes Result, N(), 0, False

    Else
        ' ------------------------------------------------------
        ' Exact decimal division.
        '
        ' We need:
        '
        '       N / 10^k
        '
        ' Instead compute the fixed-point integer
        '
        '       Q = floor(N * 256^extraBytes / 10^k)
        '
        ' with several bytes beyond the 224-bit target.  The
        ' discarded decimal-division remainder is carried into
        ' RoundExactBytes as an external sticky bit, making the
        ' final rounding true round-to-nearest-even.
        ' ------------------------------------------------------
        Dim k As Long
        k = -lDecExp

        ' log_256(10) = log(10) / log(256) ~= 0.415241...
        '
        ' Choose enough binary fractional bytes that Q always has
        ' comfortably more than SIG_LEN significant bytes, even
        ' for values much smaller than 1.
        Dim deficitDigits As Long
        deficitDigits = k - (Len(s) - 1)
        If deficitDigits < 0 Then deficitDigits = 0

        Dim extraBytes As Long
        extraBytes = SIG_BYTES + 4 + _
                     CLng(Int(CDbl(deficitDigits) * 0.4152410118609203 + 0.9999999999999999))

        Dim oldUB As Long
        oldUB = UBound(N)

        ReDim Preserve N(0 To oldUB + extraBytes)

        ' ReDim Preserve zero-initializes the appended bytes, so
        ' this is exactly multiplication by 256^extraBytes.

        Dim blExternalSticky As Boolean = False
        Dim rem10 As Long

        For i = 1 To k
            rem10 = BigByte_DivSmall(N(), 10)
            If rem10 <> 0 Then blExternalSticky = True
        Next

        RoundExactBytes Result, N(), -8 * extraBytes, blExternalSticky
    End If

    If blNeg AndAlso Not IsZero_F(Result) Then
        Result.sign = 1
    End If
End Sub

' ==============================================================
' COMPARE   -1=A<B, 0=A=B, +1=A>B
' ==============================================================
Function BigFloat_Compare(ByRef A As BigFloat, ByRef B As BigFloat) As mylong
    If IsNaN_F(A) OrElse IsNaN_F(B) Then Return 0

    Dim zeroA As Boolean = IsZero_F(A)
    Dim zeroB As Boolean = IsZero_F(B)

    If zeroA Then
        If zeroB Then Return 0
        If IsNeg_F(B) Then Return 1 Else Return -1
    End If

    If zeroB Then
        If IsNeg_F(A) Then Return -1 Else Return 1
    End If

    Dim blNegA As Boolean = IsNeg_F(A)
    Dim blNegB As Boolean = IsNeg_F(B)

    If blNegA <> blNegB Then
        If blNegA Then Return -1 Else Return 1
    End If

    Dim lExpA As Long = GetExp(A)
    Dim lExpB As Long = GetExp(B)
    Dim iCmp As mylong = 0
    Dim iSig As mylong

    If lExpA > lExpB Then
        iCmp = 1
    ElseIf lExpA < lExpB Then
        iCmp = -1
    Else
        ' Limb-wise unsigned comparison; aSig(0) is most significant.
        For iSig = 0 To SIG_LEN - 1
            If A.aSig(iSig) > B.aSig(iSig) Then
                iCmp = 1
                Exit For
            ElseIf A.aSig(iSig) < B.aSig(iSig) Then
                iCmp = -1
                Exit For
            End If
        Next
    End If

    If blNegA Then iCmp = -iCmp
    Return iCmp
End Function

' ==============================================================
' ASSIGN / CLEAR / SETNAN
' ==============================================================
Sub BigFloat_Clear(ByRef F As BigFloat)
    F.sign = 0
    F.bIsNaN = 0
    F.exponent = 0
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        F.aSig(i) = 0
    Next
End Sub

Sub BigFloat_Assign(ByRef Dst As BigFloat, ByRef Src As BigFloat)
    Dst.sign = Src.sign
    Dst.bIsNaN = Src.bIsNaN
    Dst.exponent = Src.exponent
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        Dst.aSig(i) = Src.aSig(i)
    Next
End Sub

Sub BigFloat_SetNaN(ByRef F As BigFloat)
    BigFloat_Clear F
    F.bIsNaN = 1
End Sub

' ==============================================================
' HEADER HELPERS
' ==============================================================
Function GetExp(ByRef F As BigFloat) As Long
    Return F.exponent
End Function

Sub SetExp(ByRef F As BigFloat, ByVal lE As Long)
    F.exponent = lE
End Sub

Function IsNaN_F(ByRef F As BigFloat) As Boolean
    Return (F.bIsNaN <> 0)
End Function

Function IsNeg_F(ByRef F As BigFloat) As Boolean
    Return (F.sign <> 0)
End Function

Function IsZero_F(ByRef F As BigFloat) As Boolean
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        If F.aSig(i) <> 0 Then Return False
    Next
    Return True
End Function

' ==============================================================
' SIGNIFICAND BYTE ACCESSORS
'
' Storage is seven big-endian 32-bit limbs.  These accessors keep
' the original base-256 algorithms numerically unchanged.
' ==============================================================
Function SigGetByte(ByRef F As BigFloat, ByVal iByte As Long) As UByte
    Dim iLimb As Long = iByte Shr 2
    Dim iShift As Long = (3 - (iByte And 3)) Shl 3
    Return CUByte((F.aSig(iLimb) Shr iShift) And &HFFUL)
End Function

Sub SigSetByte(ByRef F As BigFloat, ByVal iByte As Long, ByVal v As UByte)
    Dim iLimb As Long = iByte Shr 2
    Dim iShift As Long = (3 - (iByte And 3)) Shl 3
    Dim mask As ULong = &HFFUL Shl iShift

    F.aSig(iLimb) = (F.aSig(iLimb) And Not mask) Or (CULng(v) Shl iShift)
End Sub

' ==============================================================
' NORMALIZE
' ==============================================================
Sub Normalize(ByRef F As BigFloat)
    If IsNaN_F(F) Then Exit Sub
    If IsZero_F(F) Then
        BigFloat_Clear F
        Exit Sub
    End If

    Dim first As Long
    For first = 0 To SIG_LEN - 1
        If F.aSig(first) <> 0 Then Exit For
    Next

    Dim shiftBits As Long
    shiftBits = first * 32 + LeadingZeroBits32(F.aSig(first))
    If shiftBits = 0 Then Exit Sub

    Dim limbShift As Long = shiftBits Shr 5
    Dim bitShift As Long = shiftBits And 31
    Dim T(0 To SIG_LEN - 1) As ULong
    Dim i As Long, src As Long

    If bitShift = 0 Then
        For i = 0 To SIG_LEN - 1
            src = i + limbShift
            If src < SIG_LEN Then T(i) = F.aSig(src)
        Next
    Else
        For i = 0 To SIG_LEN - 1
            src = i + limbShift
            If src < SIG_LEN Then
                T(i) = F.aSig(src) Shl bitShift
                If src + 1 < SIG_LEN Then
                    T(i) Or= F.aSig(src + 1) Shr (32 - bitShift)
                End If
            End If
        Next
    End If

    For i = 0 To SIG_LEN - 1
        F.aSig(i) = T(i)
    Next
    F.exponent -= shiftBits
End Sub

' ==============================================================
' Native base-2^32 limb helpers.
' Internal arrays use little-endian limb order because carry,
' multiplication and division are simpler in that orientation.
' Storage in BigFloat remains big-endian aSig(0)=MS limb.
' ==============================================================
Sub SigToLE(ByRef F As BigFloat, A() As ULong)
    ReDim A(0 To SIG_LEN - 1) As ULong
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        A(i) = F.aSig(SIG_LEN - 1 - i)
    Next
End Sub

Sub SigFromLE(ByRef F As BigFloat, A() As ULong)
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        If i <= UBound(A) Then
            F.aSig(SIG_LEN - 1 - i) = A(i)
        Else
            F.aSig(SIG_LEN - 1 - i) = 0
        End If
    Next
End Sub

Function SignificantBytesLE(A() As ULong) As Long
    Dim h As Long
    h = UBound(A)

    Do While h >= LBound(A) AndAlso A(h) = 0
        h -= 1
    Loop

    If h < LBound(A) Then Return 0

    Dim x As ULong
    x = A(h)

    Dim topBytes As Long
    If (x And &HFF000000UL) <> 0 Then
        topBytes = 4
    ElseIf (x And &H00FF0000UL) <> 0 Then
        topBytes = 3
    ElseIf (x And &H0000FF00UL) <> 0 Then
        topBytes = 2
    Else
        topBytes = 1
    End If

    Return h * 4 + topBytes
End Function

Function GetByteLE(A() As ULong, ByVal byteIndex As Long) As UByte
    If byteIndex < 0 Then Return 0

    Dim limb As Long
    limb = byteIndex Shr 2

    If limb > UBound(A) Then Return 0

    Dim sh As Long
    sh = (byteIndex And 3) Shl 3

    Return CUByte((A(limb) Shr sh) And &HFFUL)
End Function

Function AnyLowBytesNonZeroLE(A() As ULong, ByVal byteCount As Long) As Boolean
    If byteCount <= 0 Then Return False

    Dim fullLimbs As Long
    Dim remBytes As Long
    Dim i As Long

    fullLimbs = byteCount Shr 2
    remBytes = byteCount And 3

    For i = 0 To fullLimbs - 1
        If i <= UBound(A) Then
            If A(i) <> 0 Then Return True
        End If
    Next

    If remBytes <> 0 AndAlso fullLimbs <= UBound(A) Then
        Dim mask As ULong

        Select Case remBytes
        Case 1
            mask = &H000000FFUL
        Case 2
            mask = &H0000FFFFUL
        Case Else
            mask = &H00FFFFFFUL
        End Select

        If (A(fullLimbs) And mask) <> 0 Then Return True
    End If

    Return False
End Function

Sub ShiftLeftBytesLE(Src() As ULong, Dst() As ULong, ByVal shiftBytes As Long)
    Dim limbShift As Long
    Dim bitShift As Long
    limbShift = shiftBytes Shr 2
    bitShift = (shiftBytes And 3) Shl 3

    ReDim Dst(0 To UBound(Src) + limbShift + 1) As ULong

    Dim i As Long
    Dim t As ULongInt
    Dim carry As ULongInt = 0

    If bitShift = 0 Then
        For i = 0 To UBound(Src)
            Dst(i + limbShift) = Src(i)
        Next
        Exit Sub
    End If

    For i = 0 To UBound(Src)
        t = (CULngInt(Src(i)) Shl bitShift) Or carry
        Dst(i + limbShift) = CULng(t And &HFFFFFFFFULL)
        carry = t Shr 32
    Next

    Dst(UBound(Src) + limbShift + 1) = CULng(carry)
End Sub

Sub ShiftRightBytesTo7LE(Src() As ULong, Dst() As ULong, ByVal shiftBytes As Long)
    ReDim Dst(0 To SIG_LEN - 1) As ULong

    Dim limbShift As Long
    Dim bitShift As Long
    limbShift = shiftBytes Shr 2
    bitShift = (shiftBytes And 3) Shl 3

    Dim i As Long
    Dim lo As ULong, hi As ULong

    For i = 0 To SIG_LEN - 1
        If i + limbShift <= UBound(Src) Then
            lo = Src(i + limbShift)
        Else
            lo = 0
        End If

        If bitShift = 0 Then
            Dst(i) = lo
        Else
            If i + limbShift + 1 <= UBound(Src) Then
                hi = Src(i + limbShift + 1)
            Else
                hi = 0
            End If

            Dst(i) = (lo Shr bitShift) _
                   Or (hi Shl (32 - bitShift))
        End If
    Next
End Sub

' ==============================================================
' Round an exact non-negative little-endian base-2^32 integer to
' the 224-bit BigFloat significand using round-nearest-even.
'
' Exact value = integer(A) * 256^lBaseExp.
' ==============================================================
Sub RoundExactLimbs(ByRef Result As BigFloat, A() As ULong, _
                    ByVal lBaseExp As Long, _
                    ByVal blExternalSticky As Boolean)
    BigFloat_Clear Result

    Dim hi As Long = UBound(A)
    Do While hi >= LBound(A) AndAlso A(hi) = 0
        hi -= 1
    Loop
    If hi < LBound(A) Then Exit Sub

    Dim highBit As Long
    highBit = 31 - LeadingZeroBits32(A(hi))

    Dim nBits As Long
    nBits = hi * 32 + highBit + 1

    Dim T(0 To SIG_LEN - 1) As ULong
    Dim i As Long, src As Long
    Dim lExp As Long

    If nBits <= SIG_BITS Then
        Dim leftShift As Long = SIG_BITS - nBits
        Dim limbShift As Long = leftShift Shr 5
        Dim bitShift As Long = leftShift And 31
        Dim carry As ULongInt = 0
        Dim x As ULongInt

        For i = 0 To hi
            src = i + limbShift
            If src < SIG_LEN Then
                x = (CULngInt(A(i)) Shl bitShift) Or carry
                T(src) = CULng(x And &HFFFFFFFFULL)
                carry = x Shr 32
            End If
        Next
        If bitShift <> 0 Then
            src = hi + limbShift + 1
            If src < SIG_LEN Then T(src) = CULng(carry)
        End If

        For i = 0 To SIG_LEN - 1
            Result.aSig(SIG_LEN - 1 - i) = T(i)
        Next
        Result.exponent = lBaseExp - leftShift
        Exit Sub
    End If

    Dim drop As Long = nBits - SIG_BITS
    Dim rLimb As Long = drop Shr 5
    Dim rBits As Long = drop And 31
    Dim lo As ULong, hh As ULong

    For i = 0 To SIG_LEN - 1
        src = i + rLimb
        If src <= UBound(A) Then lo = A(src) Else lo = 0
        If rBits = 0 Then
            T(i) = lo
        Else
            If src + 1 <= UBound(A) Then hh = A(src + 1) Else hh = 0
            T(i) = (lo Shr rBits) Or (hh Shl (32 - rBits))
        End If
    Next

    Dim guard As Boolean = False
    Dim sticky As Boolean = blExternalSticky
    Dim gIndex As Long = drop - 1
    Dim gLimb As Long = gIndex Shr 5
    Dim gBit As Long = gIndex And 31
    guard = (((A(gLimb) Shr gBit) And 1UL) <> 0)

    Dim stickyBits As Long = drop - 1
    If stickyBits > 0 Then
        Dim full As Long = stickyBits Shr 5
        Dim remBits As Long = stickyBits And 31
        For i = 0 To full - 1
            If A(i) <> 0 Then sticky = True : Exit For
        Next
        If (Not sticky) AndAlso remBits <> 0 Then
            Dim mask As ULong
            mask = (1UL Shl remBits) - 1UL
            If (A(full) And mask) <> 0 Then sticky = True
        End If
    End If

    Dim roundUp As Boolean
    roundUp = guard AndAlso (sticky OrElse CBool((T(0) And 1UL) <> 0))
    lExp = lBaseExp + drop

    If roundUp Then
        Dim c As ULongInt = 1
        Dim u As ULongInt
        For i = 0 To SIG_LEN - 1
            u = CULngInt(T(i)) + c
            T(i) = CULng(u And &HFFFFFFFFULL)
            c = u Shr 32
            If c = 0 Then Exit For
        Next
        If c <> 0 Then
            For i = 0 To SIG_LEN - 1
                Result.aSig(i) = 0
            Next
            Result.aSig(0) = &H80000000UL
            Result.exponent = lExp + 1
            Exit Sub
        End If
    End If

    For i = 0 To SIG_LEN - 1
        Result.aSig(SIG_LEN - 1 - i) = T(i)
    Next
    Result.exponent = lExp
End Sub

' ==============================================================
' Round an exact non-negative big-endian base-256 integer to the
' BigFloat 28-byte significand using round-to-nearest, ties-even.
'
' Exact value represented by A():
'
'       integer(A) * 256^lBaseExp
' ==============================================================
Sub RoundExactBytes(ByRef Result As BigFloat, A() As Long, ByVal lBaseExp As Long, _
                    ByVal blExternalSticky As Boolean)
    Dim first As Long = LBound(A)
    Dim last As Long = UBound(A)
    Do While first <= last AndAlso A(first) = 0
        first += 1
    Loop
    If first > last Then
        BigFloat_Clear Result
        Exit Sub
    End If

    Dim nBytes As Long = last - first + 1
    Dim nLimbs As Long = (nBytes + 3) Shr 2
    Dim T() As ULong
    ReDim T(0 To nLimbs - 1) As ULong

    Dim i As Long, position As Long, li As Long, sh As Long
    For i = last To first Step -1
        position = last - i
        li = position Shr 2
        sh = (position And 3) Shl 3
        T(li) Or= CULng(A(i) And 255) Shl sh
    Next

    RoundExactLimbs Result, T(), lBaseExp, blExternalSticky
End Sub

' ==============================================================
' INTERNAL SIGADD   requires expA >= expB
'
' For exponent gaps <= 28, construct the exact aligned integer and
' round once.  For gaps >= 29, B is less than 1/256 ulp of A and
' cannot change a round-to-nearest result.
' ==============================================================
Sub SigAdd(ByRef Result As BigFloat, ByRef A As BigFloat, ByRef B As BigFloat, _
           ByVal lExpA As Long, ByVal lExpB As Long)
    Dim d As Long = lExpA - lExpB
    If d > SIG_BITS Then
        BigFloat_Assign Result, A
        Result.sign = 0
        Exit Sub
    End If

    Dim AL(0 To SIG_LEN - 1) As ULong
    Dim BL(0 To SIG_LEN - 1) As ULong
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        AL(i) = A.aSig(SIG_LEN - 1 - i)
        BL(i) = B.aSig(SIG_LEN - 1 - i)
    Next

    Dim W(0 To 2 * SIG_LEN + 1) As ULong
    Dim limbShift As Long = d Shr 5
    Dim bitShift As Long = d And 31
    Dim carryS As ULongInt = 0, tS As ULongInt

    If bitShift = 0 Then
        For i = 0 To SIG_LEN - 1
            W(i + limbShift) = AL(i)
        Next
    Else
        For i = 0 To SIG_LEN - 1
            tS = (CULngInt(AL(i)) Shl bitShift) Or carryS
            W(i + limbShift) = CULng(tS And &HFFFFFFFFULL)
            carryS = tS Shr 32
        Next
        W(SIG_LEN + limbShift) = CULng(carryS)
    End If

    Dim n As Long = SIG_LEN + limbShift
    Dim carry As ULongInt = 0, bv As ULongInt, t As ULongInt
    For i = 0 To n
        If i < SIG_LEN Then bv = BL(i) Else bv = 0
        t = CULngInt(W(i)) + bv + carry
        W(i) = CULng(t And &HFFFFFFFFULL)
        carry = t Shr 32
    Next
    W(n + 1) = CULng(carry)

    RoundExactLimbs Result, W(), lExpB, False
End Sub

' ==============================================================
' INTERNAL SIGSUB   requires |A| >= |B| and expA >= expB
'
' Exact aligned subtraction is important because cancellation can
' expose low-order bytes that a guard-only scheme would lose.
' ==============================================================
Sub SigSub(ByRef Result As BigFloat, ByRef A As BigFloat, ByRef B As BigFloat, _
           ByVal lExpA As Long, ByVal lExpB As Long)
    Const BASE64 As ULongInt = 4294967296ULL
    Dim d As Long = lExpA - lExpB
    If d > SIG_BITS Then
        BigFloat_Assign Result, A
        Result.sign = 0
        Exit Sub
    End If

    Dim AL(0 To SIG_LEN - 1) As ULong
    Dim BL(0 To SIG_LEN - 1) As ULong
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        AL(i) = A.aSig(SIG_LEN - 1 - i)
        BL(i) = B.aSig(SIG_LEN - 1 - i)
    Next

    Dim W(0 To 2 * SIG_LEN + 1) As ULong
    Dim limbShift As Long = d Shr 5
    Dim bitShift As Long = d And 31
    Dim carryS As ULongInt = 0, tS As ULongInt
    If bitShift = 0 Then
        For i = 0 To SIG_LEN - 1
            W(i + limbShift) = AL(i)
        Next
    Else
        For i = 0 To SIG_LEN - 1
            tS = (CULngInt(AL(i)) Shl bitShift) Or carryS
            W(i + limbShift) = CULng(tS And &HFFFFFFFFULL)
            carryS = tS Shr 32
        Next
        W(SIG_LEN + limbShift) = CULng(carryS)
    End If

    Dim borrow As ULongInt = 0
    Dim av As ULongInt, bv As ULongInt
    Dim n As Long = SIG_LEN + limbShift
    For i = 0 To n
        av = CULngInt(W(i))
        If i < SIG_LEN Then bv = CULngInt(BL(i)) Else bv = 0
        bv += borrow
        If av < bv Then
            W(i) = CULng(BASE64 + av - bv)
            borrow = 1
        Else
            W(i) = CULng(av - bv)
            borrow = 0
        End If
    Next

    RoundExactLimbs Result, W(), lExpB, False
End Sub

' ==============================================================
' TOSTRING
' ==============================================================
Function BigFloat_ToString(ByRef F As BigFloat) As String
    If IsNaN_F(F) Then Return "NaN"
    If IsZero_F(F) Then Return "0"

    Const PREC As mylong = -Int(-SIG_LEN*9.632959861247398)
    Dim signPrefix As String
    If IsNeg_F(F) Then signPrefix = "-"

    Dim DEC() As mylong
    BigDec_Init DEC()
    Dim i As Long
    For i = 0 To SIG_BYTES - 1
        BigDec_MulSmall DEC(), 256
        BigDec_AddSmall DEC(), SigGetByte(F, i)
    Next

    Dim E As Long = F.exponent
    Dim e10 As Long = 0
    Dim j As Long
    If E > 0 Then
        For j = 1 To E
            BigDec_MulSmall DEC(), 2
        Next
    ElseIf E < 0 Then
        For j = 1 To -E
            BigDec_MulSmall DEC(), 5
        Next
        e10 += E
    End If

    Dim sDec As String = BigDec_ToString(DEC())
    Dim L As Long = Len(sDec)
    Dim q As Long = (L - 1) + e10
    Dim mant As String
    If L >= PREC Then
        mant = Left(sDec, PREC)
    Else
        mant = sDec & String(PREC - L, "0")
    End If

    Dim sOut As String = Left(mant, 1)
    If Len(mant) > 1 Then sOut &= "." & Mid(mant, 2)
    If q <> 0 Then sOut &= "e" & Trim(Str(q))
    Return signPrefix & sOut
End Function


' ==============================================================
' Dynamic big-endian base-256 integer helpers used by FromString.
' Each element is one byte value 0..255, stored in a Long.
' ==============================================================

Sub BigByte_MulSmall(A() As Long, ByVal m As Long)
    Dim i As Long
    Dim carry As Long = 0
    Dim t As Long

    For i = UBound(A) To LBound(A) Step -1
        t = A(i) * m + carry
        A(i) = t And 255
        carry = t \ 256
    Next

    Do While carry <> 0
        Dim oldUB As Long
        oldUB = UBound(A)

        ReDim Preserve A(0 To oldUB + 1)

        For i = oldUB + 1 To 1 Step -1
            A(i) = A(i - 1)
        Next

        A(0) = carry And 255
        carry \= 256
    Loop
End Sub

Sub BigByte_AddSmall(A() As Long, ByVal v As Long)
    Dim i As Long
    Dim carry As Long = v
    Dim t As Long

    i = UBound(A)

    Do While i >= LBound(A) AndAlso carry <> 0
        t = A(i) + carry
        A(i) = t And 255
        carry = t \ 256
        i -= 1
    Loop

    Do While carry <> 0
        Dim oldUB As Long
        oldUB = UBound(A)

        ReDim Preserve A(0 To oldUB + 1)

        For i = oldUB + 1 To 1 Step -1
            A(i) = A(i - 1)
        Next

        A(0) = carry And 255
        carry \= 256
    Loop
End Sub

Function BigByte_DivSmall(A() As Long, ByVal d As Long) As Long
    Dim i As Long
    Dim carry As Long = 0
    Dim t As Long

    For i = LBound(A) To UBound(A)
        t = carry * 256 + A(i)
        A(i) = t \ d
        carry = t Mod d
    Next

    BigByte_DivSmall = carry
End Function

' ==============================================================
' BigDec: dynamic decimal digit array, LSB at index zero
' ==============================================================
Sub BigDec_Init(A() As mylong)
    ReDim A(0)
    A(0) = 0
End Sub

Sub BigDec_TrimMSZ(A() As mylong)
    Dim i As Long

    For i = UBound(A) To 1 Step -1
        If A(i) <> 0 Then Exit For
        ReDim Preserve A(i - 1)
    Next
End Sub

Sub BigDec_MulSmall(A() As mylong, ByVal m As mylong)
    If m = 0 Then
        ReDim A(0)
        A(0) = 0
        Exit Sub
    End If

    Dim carry As Long = 0
    Dim i As Long, prod As Long

    For i = 0 To UBound(A)
        prod = CLng(A(i)) * m + carry
        A(i) = prod Mod 10
        carry = prod \ 10
    Next

    Do While carry > 0
        ReDim Preserve A(UBound(A) + 1)
        A(UBound(A)) = carry Mod 10
        carry \= 10
    Loop
End Sub

Sub BigDec_AddSmall(A() As mylong, ByVal addVal As mylong)
    Dim carry As Long = addVal
    Dim i As Long = 0
    Dim s As Long

    Do While carry > 0
        If i > UBound(A) Then
            ReDim Preserve A(i)
            A(i) = 0
        End If

        s = A(i) + (carry Mod 10)
        A(i) = s Mod 10
        carry = (carry \ 10) + (s \ 10)

        i += 1
    Loop
End Sub

Function BigDec_ToString(A() As mylong) As String
    BigDec_TrimMSZ A()

    Dim i As Long
    Dim sb As String

    For i = UBound(A) To 0 Step -1
        sb &= Trim(Str(A(i)))
    Next

    If sb = "" Then sb = "0"

    BigDec_ToString = sb
End Function

' ==============================================================
' IsExpNeg
' ==============================================================
Function IsExpNeg(ByRef F As BigFloat) As Boolean
    Return (F.exponent < 0)
End Function

'===============================================================
' ==============================================================
' BigFloat_Truncate
'
' Result = integer part of X, truncating toward zero.
'
' Representation:
'
'       X = M * 256^E
'
' M contains 28 bytes = 7 ULong limbs.
'
' The radix point therefore occurs after:
'
'       nIntegerBytes = 28 + E
'
' bytes of the significand.
'
' Alias safe:
'
'       BigFloat_Truncate A, A
' ==============================================================
Sub BigFloat_Truncate(ByRef Result As BigFloat, ByRef X As BigFloat)
    If IsNaN_F(X) Then BigFloat_SetNaN Result : Exit Sub
    If IsZero_F(X) Then BigFloat_Clear Result : Exit Sub

    Dim E As Long = X.exponent
    If E >= 0 Then BigFloat_Assign Result, X : Exit Sub

    Dim cut As Long = -E
    If cut >= SIG_BITS Then BigFloat_Clear Result : Exit Sub

    BigFloat_Assign Result, X
    Dim full As Long = cut Shr 5
    Dim remBits As Long = cut And 31
    Dim i As Long, idx As Long

    For i = 0 To full - 1
        Result.aSig(SIG_LEN - 1 - i) = 0
    Next
    If remBits <> 0 Then
        idx = SIG_LEN - 1 - full
        Result.aSig(idx) And= Not ((1UL Shl remBits) - 1UL)
    End If

    If IsZero_F(Result) Then BigFloat_Clear Result
End Sub

Function BigFloat_TruncatedIsOdd(ByRef X As BigFloat) As Boolean
    If IsNaN_F(X) OrElse IsZero_F(X) Then Return False
    If X.exponent > 0 Then Return False

    Dim bitIndex As Long = -X.exponent
    If bitIndex < 0 OrElse bitIndex >= SIG_BITS Then Return False

    Dim limbIndex As Long = SIG_LEN - 1 - (bitIndex Shr 5)
    Dim bitInLimb As Long = bitIndex And 31
    Return (((X.aSig(limbIndex) Shr bitInLimb) And 1UL) <> 0)
End Function

' ==============================================================
' NEGATE
' ==============================================================
Sub BigFloat_Negate(ByRef Result As BigFloat, ByRef X As BigFloat)

    BigFloat_Assign Result, X

    ' Keep canonical zero positive.
    If Not IsZero_F(Result) Then
        Result.sign Xor= 1
    End If

End Sub


' ==============================================================
' ABSOLUTE VALUE
' ==============================================================
Sub BigFloat_Abs(ByRef Result As BigFloat, ByRef X As BigFloat)

    BigFloat_Assign Result, X
    Result.sign = 0

End Sub

' ==============================================================
' MULTIPLY BY 2
'
' Internal arrays are LITTLE-ENDIAN:
'
'       A(0) = least significant 32-bit limb
'
' Exact value:
'
'       (2*M) * 256^E
'
' RoundExactLimbs handles the rare 225-bit result using RNE.
' ==============================================================
Sub BigFloat_Mul2(ByRef Result As BigFloat, ByRef X As BigFloat)
    BigFloat_Assign Result, X
    If IsNaN_F(Result) OrElse IsZero_F(Result) Then Exit Sub
    Result.exponent += 1
End Sub

' ==============================================================
' DIVIDE BY 2
'
'       X = M * 256^E
'
' therefore:
'
'       X/2 = M/2 * 256^E
'
' but M/2 may not be an integer.
'
' Instead form the EXACT equivalent:
'
'       X/2 = (128*M) * 256^(E-1)
'
' because:
'
'       128 / 256 = 1/2
'
' This preserves all bits before the final BigFloat RNE rounding.
' ==============================================================
Sub BigFloat_Div2(ByRef Result As BigFloat, ByRef X As BigFloat)
    BigFloat_Assign Result, X
    If IsNaN_F(Result) OrElse IsZero_F(Result) Then Exit Sub
    Result.exponent -= 1
End Sub

' ==============================================================
' ARITHMETIC AGAINST A 64-BIT INTEGER SCALAR
' ==============================================================

' ----------------------------------------------------------
' ADD / SUBTRACT -- build the scalar as a BigFloat (O(1), just
' sets one or two limbs -- see BigFloat_FromLongInt/FromULongInt)
' and hand off to the already-correct general routines.
' ----------------------------------------------------------
Sub BigFloat_AddI64(ByRef Result As BigFloat, ByRef X As BigFloat, ByVal v As LongInt)
    Dim T As BigFloat
    BigFloat_FromLongInt T, v
    BigFloat_Add Result, X, T
End Sub

Sub BigFloat_AddU64(ByRef Result As BigFloat, ByRef X As BigFloat, ByVal v As ULongInt)
    Dim T As BigFloat
    BigFloat_FromULongInt T, v
    BigFloat_Add Result, X, T
End Sub

Sub BigFloat_SubtractI64(ByRef Result As BigFloat, ByRef X As BigFloat, ByVal v As LongInt)
    Dim T As BigFloat
    BigFloat_FromLongInt T, v
    BigFloat_Subtract Result, X, T
End Sub

Sub BigFloat_SubtractU64(ByRef Result As BigFloat, ByRef X As BigFloat, ByVal v As ULongInt)
    Dim T As BigFloat
    BigFloat_FromULongInt T, v
    BigFloat_Subtract Result, X, T
End Sub

' ----------------------------------------------------------
' MULTIPLY
'
' A dedicated 14x(1 or 2)-limb product instead of routing
' through the full 14x14 multiply core: X's 224-bit significand
' times a plain (unscaled) 64-bit integer only needs 14 to 28
' limb multiplies, versus 162-196 for a full Multiply.  The
' exponent is unchanged (mag carries no base-256 scale of its
' own) -- only the significand grows, exactly as in BigFloat_Mul2.
' ----------------------------------------------------------
Sub BigFloat_MultiplyU64(ByRef Result As BigFloat, ByRef X As BigFloat, ByVal mag As ULongInt)
    If IsNaN_F(X) Then
        BigFloat_SetNaN Result
        Exit Sub
    End If

    If IsZero_F(X) OrElse mag = 0 Then
        BigFloat_Clear Result
        Exit Sub
    End If

    Dim blNeg As Boolean
    blNeg = IsNeg_F(X)

    Dim XL(0 To SIG_LEN - 1) As ULong
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        XL(i) = X.aSig(SIG_LEN - 1 - i)
    Next

    Dim ML(0 To 1) As ULong
    ML(0) = CULng(mag And &HFFFFFFFFULL)
    ML(1) = CULng(mag Shr 32)

    Dim nm As Long
    If ML(1) <> 0 Then
        nm = 2
    Else
        nm = 1
    End If

    ' Exact product needs SIG_LEN+nm limbs (<=16); one spare limb
    ' kept for consistency with the defensive margin used elsewhere.
    Dim P(0 To SIG_LEN + 2) As ULong

    LimbMulAccumulate XL(), SIG_LEN, ML(), nm, P()

    RoundExactLimbs Result, P(), GetExp(X), False

    If blNeg AndAlso Not IsZero_F(Result) Then
        Result.sign = 1
    End If
End Sub

Sub BigFloat_MultiplyI64(ByRef Result As BigFloat, ByRef X As BigFloat, ByVal v As LongInt)
    Dim mag As ULongInt
    Dim neg As Boolean

    If v < 0 Then
        neg = True
        ' Avoid overflow at LONGINT_MIN, same trick as BigFloat_FromLongInt.
        mag = CULngInt(-(v + 1LL)) + 1ULL
    Else
        neg = False
        mag = CULngInt(v)
    End If

    BigFloat_MultiplyU64 Result, X, mag

    If neg AndAlso Not IsZero_F(Result) Then
        Result.sign Xor= 1
    End If
End Sub

' ----------------------------------------------------------
' DIVIDE
'
' Dedicated small-divisor long division instead of routing through
' the general Div224x224 (which is hardcoded to a fixed SIG_LEN-
' wide divisor regardless of how many limbs are actually nonzero,
' so it wastes O(SIG_LEN) work per step that this doesn't need).
'
' Numerator is X's significand shifted left by exactly 64 bits (2
' limbs) -- NOT by the full SIG_BYTES the general divide uses.
' Since X's significand is always byte-normalized (top byte
' nonzero) and mag is at most 64 bits, a 64-bit shift is exactly
' enough to guarantee the quotient never comes up short of
' SIG_BYTES significant bytes: numerator >= Mx*2^64 with Mx in
' [256^(SIG_BYTES-1), 256^SIG_BYTES), so quotient > Mx*2^64/2^64 =
' Mx >= 256^(SIG_BYTES-1) always, regardless of mag.  (Verified
' numerically, but also true by that inequality alone.)  It also
' means an exact round-to-nearest-even TIE is structurally
' impossible here: the achievable remainders of Mx*2^64 mod mag
' are exactly the multiples of g = gcd(2^64, mag), and mag/2 is
' never one of them, since mag/g is always odd.  The tie-break
' code below is kept anyway, both for defensiveness and because it
' costs nothing measurable.
'
' mag=1 divisor: plain single-limb long division sweep.
' mag>2^32-1 divisor: Knuth Algorithm D specialized to a 2-limb
' divisor (same structure as Div224x224, just parameterized down
' to m=2 instead of m=SIG_LEN, which is what makes it cheap).
' ----------------------------------------------------------
Sub BigFloat_DivideU64(ByRef Result As BigFloat, ByRef X As BigFloat, ByVal mag As ULongInt)
    If IsNaN_F(X) OrElse mag = 0 Then
        BigFloat_SetNaN Result
        Exit Sub
    End If

    If IsZero_F(X) Then
        BigFloat_Clear Result
        Exit Sub
    End If

    Dim blNeg As Boolean
    blNeg = IsNeg_F(X)

    Const BASE64 As ULongInt = 4294967296ULL
    Const MASK64 As ULongInt = &HFFFFFFFFULL

    ' numerator = Mx * 2^64: 16 little-endian limbs, low 2 are zero.
    Dim numerator(0 To SIG_LEN + 1) As ULong
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        numerator(i + 2) = X.aSig(SIG_LEN - 1 - i)
    Next

    Dim d0 As ULong, d1 As ULong
    d0 = CULng(mag And MASK64)
    d1 = CULng(mag Shr 32)

    Dim Q(0 To SIG_LEN + 1) As ULong
    Dim R As ULongInt

    If d1 = 0 Then
        ' Single-limb divisor: remainder always < d0 <= 2^32-1, so it
        ' fits in a limb and each step is one native 64-bit div/mod.
        Dim r As ULongInt = 0
        Dim cur As ULongInt
        Dim k As Long
        For k = SIG_LEN + 1 To 0 Step -1
            cur = (r Shl 32) Or CULngInt(numerator(k))
            Q(k) = CULng(cur \ CULngInt(d0))
            r = cur Mod CULngInt(d0)
        Next
        R = r
    Else
        ' Two-limb divisor: Knuth Algorithm D, m=2.
        Dim s As Long
        s = LeadingZeroBits32(d1)

        Dim vn0 As ULong, vn1 As ULong
        If s = 0 Then
            vn0 = d0 : vn1 = d1
        Else
            vn1 = CULng(((CULngInt(d1) Shl s) Or (CULngInt(d0) Shr (32 - s))) And MASK64)
            vn0 = CULng((CULngInt(d0) Shl s) And MASK64)
        End If

        Dim un(0 To SIG_LEN + 2) As ULong   ' numerator (16 limbs) + 1 normalization limb
        Dim carryN As ULongInt
        If s = 0 Then
            For i = 0 To SIG_LEN + 1
                un(i) = numerator(i)
            Next
        Else
            carryN = 0
            Dim tN As ULongInt
            For i = 0 To SIG_LEN + 1
                tN = (CULngInt(numerator(i)) Shl s) Or carryN
                un(i) = CULng(tN And MASK64)
                carryN = tN Shr 32
            Next
            un(SIG_LEN + 2) = CULng(carryN)
        End If

        Dim nq As Long
        nq = SIG_LEN + 1   ' (SIG_LEN+2) - 2 + 1 quotient digits, j = nq-1 downto 0

        Dim j As Long
        Dim qhat As ULongInt, rhat As ULongInt, num As ULongInt
        Dim lhs As ULongInt, rhs As ULongInt
        Dim p As ULongInt, pLow As ULongInt, borrow As ULongInt
        Dim ui As ULongInt, negative As Boolean, sum As ULongInt, carry2 As ULongInt

        For j = nq - 1 To 0 Step -1
            If un(j + 2) = vn1 Then
                qhat = BASE64 - 1
                rhat = CULngInt(un(j + 1)) + CULngInt(vn1)
            Else
                num = (CULngInt(un(j + 2)) Shl 32) Or CULngInt(un(j + 1))
                qhat = num \ CULngInt(vn1)
                rhat = num Mod CULngInt(vn1)
            End If

            Do
                If rhat >= BASE64 Then Exit Do
                lhs = qhat * CULngInt(vn0)
                rhs = (rhat Shl 32) Or CULngInt(un(j))
                If lhs <= rhs Then Exit Do
                qhat -= 1
                rhat += CULngInt(vn1)
            Loop

            borrow = 0
            p = qhat * CULngInt(vn0) + borrow
            pLow = p And MASK64
            borrow = p Shr 32
            ui = CULngInt(un(j))
            If ui < pLow Then
                un(j) = CULng(BASE64 + ui - pLow)
                borrow += 1
            Else
                un(j) = CULng(ui - pLow)
            End If

            p = qhat * CULngInt(vn1) + borrow
            pLow = p And MASK64
            borrow = p Shr 32
            ui = CULngInt(un(j + 1))
            If ui < pLow Then
                un(j + 1) = CULng(BASE64 + ui - pLow)
                borrow += 1
            Else
                un(j + 1) = CULng(ui - pLow)
            End If

            ui = CULngInt(un(j + 2))
            If ui < borrow Then
                un(j + 2) = CULng(BASE64 + ui - borrow)
                negative = True
            Else
                un(j + 2) = CULng(ui - borrow)
                negative = False
            End If

            If negative Then
                qhat -= 1
                sum = CULngInt(un(j)) + CULngInt(vn0)
                un(j) = CULng(sum And MASK64)
                carry2 = sum Shr 32
                sum = CULngInt(un(j + 1)) + CULngInt(vn1) + carry2
                un(j + 1) = CULng(sum And MASK64)
                carry2 = sum Shr 32
                un(j + 2) = CULng((CULngInt(un(j + 2)) + carry2) And MASK64)
            End If

            Q(j) = CULng(qhat)
        Next

        Dim r0 As ULong, r1 As ULong
        If s = 0 Then
            r0 = un(0) : r1 = un(1)
        Else
            r0 = CULng(((CULngInt(un(0)) Shr s) Or (CULngInt(un(1)) Shl (32 - s))) And MASK64)
            r1 = CULng(((CULngInt(un(1)) Shr s) Or (CULngInt(un(2)) Shl (32 - s))) And MASK64)
        End If
        R = CULngInt(r0) Or (CULngInt(r1) Shl 32)
    End If

    ' --- rounding: mirrors BigFloat_Divide's tail, specialized for a
    ' plain integer remainder R and divisor mag instead of arrays. ---
    Dim qBytes As Long
    qBytes = SignificantBytesLE(Q())

    Dim lNewExp As Long
    lNewExp = GetExp(X) - 64   ' numerator was shifted by 64 bits

    If qBytes > SIG_BYTES Then
        RoundExactLimbs Result, Q(), lNewExp, (R <> 0)
    Else
        ' No guard byte to read from Q itself (it's not truncated) --
        ' the whole rounding decision comes from comparing the exact
        ' remainder R against mag/2.
        Dim roundUp As Boolean = False

        If (R Shr 63) <> 0 Then
            roundUp = True   ' 2R doesn't fit in 64 bits => 2R > mag
        Else
            Dim twoR As ULongInt
            twoR = R Shl 1
            If twoR > mag Then
                roundUp = True
            ElseIf twoR = mag Then
                roundUp = ((Q(0) And 1UL) <> 0)   ' tie -> round to even (unreachable here, kept for safety)
            End If
        End If

        If roundUp Then
            Dim carryQ As ULongInt = 1
            Dim ii As Long, xq As ULongInt
            For ii = 0 To SIG_LEN + 1
                xq = CULngInt(Q(ii)) + carryQ
                Q(ii) = CULng(xq And MASK64)
                carryQ = xq Shr 32
                If carryQ = 0 Then Exit For
            Next
        End If

        ' RoundExactLimbs just repositions/pads the now-exact value --
        ' no further rounding decision left to make, hence sticky=False.
        RoundExactLimbs Result, Q(), lNewExp, False
    End If

    If blNeg AndAlso Not IsZero_F(Result) Then
        Result.sign = 1
    End If
End Sub

Sub BigFloat_DivideI64(ByRef Result As BigFloat, ByRef X As BigFloat, ByVal v As LongInt)
    Dim mag As ULongInt

    If v < 0 Then
        mag = CULngInt(-(v + 1LL)) + 1ULL   ' avoid overflow at LONGINT_MIN
    Else
        mag = CULngInt(v)
    End If

    BigFloat_DivideU64 Result, X, mag

    If v < 0 AndAlso Not IsZero_F(Result) Then
        Result.sign Xor= 1
    End If
End Sub

Function BigFloat_HighBit32(ByVal x As ULong) As Long

    Dim n As Long = 0

    If x >= &H10000UL Then
        x Shr= 16
        n += 16
    End If

    If x >= &H100UL Then
        x Shr= 8
        n += 8
    End If

    If x >= &H10UL Then
        x Shr= 4
        n += 4
    End If

    If x >= &H4UL Then
        x Shr= 2
        n += 2
    End If

    If x >= &H2UL Then
        n += 1
    End If

    Return n

End Function

' ==============================================================
' BigFloat_Frexp
'
' Decompose:
'
'       X = Mantissa * 2^BinExponent
'
' with:
'
'       0.5 <= Abs(Mantissa) < 1
'
' for nonzero X.
'
' The sign of X is retained in Mantissa.
'
' Alias safe:
'
'       BigFloat_Frexp A, A, e
'
' ==============================================================
Sub BigFloat_Frexp(ByRef X As BigFloat, _
               ByRef Mantissa As BigFloat, _
               ByRef BinExponent As Long)
    BinExponent = 0
    If IsNaN_F(X) Then BigFloat_SetNaN Mantissa : Exit Sub
    If IsZero_F(X) Then BigFloat_Clear Mantissa : Exit Sub

    BigFloat_Assign Mantissa, X
    BinExponent = X.exponent + SIG_BITS
    Mantissa.exponent = -SIG_BITS
End Sub

Sub BigFloat_FromULongInt(ByRef R As BigFloat, ByVal v As ULongInt)
    If v = 0 Then BigFloat_Clear R : Exit Sub
    Dim A(0 To 1) As ULong
    A(0) = CULng(v And &HFFFFFFFFULL)
    A(1) = CULng(v Shr 32)
    RoundExactLimbs R, A(), 0, False
End Sub

Sub BigFloat_FromLongInt(ByRef R As BigFloat, ByVal v As LongInt)

    If v >= 0 Then
        BigFloat_FromULongInt R, CULngInt(v)
        Exit Sub
    End If

    Dim mag As ULongInt
    mag = CULngInt(-(v + 1LL)) + 1ULL

    BigFloat_FromULongInt R, mag

    If Not IsZero_F(R) Then
        R.sign = 1
    End If

End Sub


Sub BigFloat_FromULong(ByRef R As BigFloat, ByVal v As ULong)
    BigFloat_FromULongInt R, CULngInt(v)
End Sub


Sub BigFloat_FromLong(ByRef R As BigFloat, ByVal v As Long)

    If v >= 0 Then
        BigFloat_FromULongInt R, CULngInt(v)
    Else
        Dim t As LongInt
        t = CLngInt(v)

        Dim mag As ULongInt
        mag = CULngInt(-(t + 1LL)) + 1ULL

        BigFloat_FromULongInt R, mag
        R.sign = 1
    End If

End Sub

Function BigFloat_ToULongInt(ByRef X As BigFloat, ByRef v As ULongInt) As Boolean
    v = 0
    If IsNaN_F(X) Then Return False
    If IsZero_F(X) Then Return True
    If IsNeg_F(X) Then Return False

    Dim highValueBit As LongInt = CLngInt(SIG_BITS - 1) + CLngInt(X.exponent)
    If highValueBit < 0 Then Return True
    If highValueBit > 63 Then Return False

    Dim k As Long
    For k = CLng(highValueBit) To 0 Step -1
        v = (v Shl 1) Or CULngInt(BigFloat_GetSigBit(X, k - X.exponent))
    Next
    Return True
End Function

Function BigFloat_ToLongInt(ByRef X As BigFloat, ByRef v As LongInt) As Boolean
    v = 0
    If IsNaN_F(X) Then Return False
    If IsZero_F(X) Then Return True

    Dim T As BigFloat
    BigFloat_Assign T, X
    T.sign = 0
    Dim mag As ULongInt
    If Not BigFloat_ToULongInt(T, mag) Then Return False

    If IsNeg_F(X) Then
        If mag > &H8000000000000000ULL Then Return False
        If mag = &H8000000000000000ULL Then
            v = -9223372036854775807LL - 1LL
        Else
            v = -CLngInt(mag)
        End If
    Else
        If mag > &H7FFFFFFFFFFFFFFFULL Then Return False
        v = CLngInt(mag)
    End If
    Return True
End Function

Function BigFloat_ToULong(ByRef X As BigFloat, _
                      ByRef v As ULong) As Boolean

    v = 0

    Dim t As ULongInt

    If Not BigFloat_ToULongInt(X, t) Then Return False
    If t > &HFFFFFFFFULL Then Return False

    v = CULng(t)

    Return True

End Function


Function BigFloat_ToLong(ByRef X As BigFloat, _
                     ByRef v As Long) As Boolean

    v = 0

    Dim t As LongInt

    If Not BigFloat_ToLongInt(X, t) Then Return False

    If t < -2147483648LL OrElse _
       t >  2147483647LL Then
        Return False
    End If

    v = CLng(t)

    Return True

End Function

Sub BigFloat_FromDouble(ByRef R As BigFloat, ByVal v As Double)
    BigFloat_Clear R
    Dim bits As ULongInt
    fb_memcopy(bits, v, 8)

    Dim neg As Boolean = ((bits Shr 63) <> 0)
    Dim expField As Long = CLng((bits Shr 52) And &H7FFULL)
    Dim fraction As ULongInt = bits And &HFFFFFFFFFFFFFULL
    If expField = &H7FF Then BigFloat_SetNaN R : Exit Sub
    If expField = 0 AndAlso fraction = 0 Then Exit Sub

    Dim M As ULongInt, e2 As Long
    If expField = 0 Then
        M = fraction
        e2 = -1074
    Else
        M = &H10000000000000ULL Or fraction
        e2 = expField - 1023 - 52
    End If

    Dim A(0 To 1) As ULong
    A(0) = CULng(M And &HFFFFFFFFULL)
    A(1) = CULng(M Shr 32)
    RoundExactLimbs R, A(), e2, False
    If neg AndAlso Not IsZero_F(R) Then R.sign = 1
End Sub

' ==============================================================
' IEEE-754 DOUBLE SUPPORT
' ==============================================================

Union BigFloat_DoubleBits
    d As Double
    u As ULongInt
End Union

Function BigFloat_SigHighBit(ByRef X As BigFloat) As Long

    Dim i As Long

    For i = 0 To SIG_LEN - 1

        If X.aSig(i) <> 0 Then

            Return _
                (SIG_LEN - 1 - i) * 32 + _
                BigFloat_HighBit32(X.aSig(i))

        End If

    Next

    Return -1

End Function

Function BigFloat_GetSigBit(ByRef X As BigFloat, _
                        ByVal bitIndex As Long) As ULong

    If bitIndex < 0 OrElse bitIndex >= SIG_LEN * 32 Then
        Return 0
    End If

    Dim limbIndex As Long
    Dim bitInLimb As Long

    limbIndex = SIG_LEN - 1 - (bitIndex Shr 5)
    bitInLimb = bitIndex And 31

    Return (X.aSig(limbIndex) Shr bitInLimb) And 1UL

End Function

Function BigFloat_AnySigBitsBelow(ByRef X As BigFloat, _
                              ByVal highBit As Long) As Boolean

    If highBit < 0 Then Return False

    If highBit >= SIG_LEN * 32 - 1 Then

        Dim i As Long

        For i = 0 To SIG_LEN - 1
            If X.aSig(i) <> 0 Then Return True
        Next

        Return False
    End If


    Dim nBits As Long
    Dim fullLimbs As Long
    Dim partialBits As Long

    nBits = highBit + 1

    fullLimbs = nBits Shr 5
    partialBits = nBits And 31


    Dim i As Long
    Dim limbIndex As Long

    For i = 0 To fullLimbs - 1

        limbIndex = SIG_LEN - 1 - i

        If X.aSig(limbIndex) <> 0 Then
            Return True
        End If

    Next


    If partialBits <> 0 Then

        limbIndex = SIG_LEN - 1 - fullLimbs

        Dim mask As ULong

        mask = (1UL Shl partialBits) - 1UL

        If (X.aSig(limbIndex) And mask) <> 0 Then
            Return True
        End If

    End If

    Return False

End Function

Function BigFloat_RoundShiftToU64(ByRef X As BigFloat, _
                              ByVal rightShift As LongInt) As ULongInt

    Dim highBit As Long
    highBit = BigFloat_SigHighBit(X)

    If highBit < 0 Then Return 0


    Dim q As ULongInt = 0
    Dim i As Long


    If rightShift <= 0 Then

        Dim leftShift As LongInt
        leftShift = -rightShift

        For i = highBit To 0 Step -1
            q = (q Shl 1) Or CULngInt(BigFloat_GetSigBit(X, i))
        Next

        If leftShift <> 0 Then
            q Shl= CLng(leftShift)
        End If

        Return q

    End If


    If rightShift <= highBit Then

        For i = highBit To CLng(rightShift) Step -1
            q = (q Shl 1) Or CULngInt(BigFloat_GetSigBit(X, i))
        Next

    End If


    Dim guard As Boolean = False
    Dim sticky As Boolean = False

    Dim guardIndex As LongInt
    guardIndex = rightShift - 1


    If guardIndex >= 0 AndAlso guardIndex <= SIG_LEN * 32 - 1 Then
        guard = (BigFloat_GetSigBit(X, CLng(guardIndex)) <> 0)
    End If


    Dim stickyHigh As LongInt
    stickyHigh = rightShift - 2

    If stickyHigh >= 0 Then

        If stickyHigh >= SIG_LEN * 32 - 1 Then

            sticky = Not IsZero_F(X)

        Else

            sticky = BigFloat_AnySigBitsBelow(X, CLng(stickyHigh))

        End If

    End If


    If guard Then

        If sticky OrElse ((q And 1ULL) <> 0) Then
            q += 1ULL
        End If

    End If

    Return q

End Function

Function BigFloat_DoubleFromBits(ByVal bits As ULongInt) As Double

    Dim U As BigFloat_DoubleBits

    U.u = bits

    Return U.d

End Function

Function BigFloat_ToDouble(ByRef X As BigFloat) As Double

    Const DOUBLE_SIGN_MASK As ULongInt = _
        &H8000000000000000ULL

    Const DOUBLE_EXP_MASK As ULongInt = _
        &H7FF0000000000000ULL

    Const DOUBLE_FRAC_MASK As ULongInt = _
        &H000FFFFFFFFFFFFFULL

    Const DOUBLE_QNAN As ULongInt = _
        &H7FF8000000000000ULL

    Const TWO52 As ULongInt = _
        &H0010000000000000ULL

    Const TWO53 As ULongInt = _
        &H0020000000000000ULL


    Dim signBits As ULongInt = 0

    If IsNeg_F(X) Then
        signBits = DOUBLE_SIGN_MASK
    End If


    If IsNaN_F(X) Then

        Return BigFloat_DoubleFromBits( _
            signBits Or DOUBLE_QNAN)

    End If


    If IsZero_F(X) Then
        Return BigFloat_DoubleFromBits(signBits)
    End If


    Dim h As Long
    h = BigFloat_SigHighBit(X)

    If h < 0 Then
        Return BigFloat_DoubleFromBits(signBits)
    End If


    Dim E As Long
    E = GetExp(X)

    Dim binExp As LongInt

    binExp = CLngInt(h) + CLngInt(E)


    If binExp >= -1022LL Then

        Dim shift As LongInt

        shift = CLngInt(h) - 52LL


        Dim sig53 As ULongInt

        sig53 = BigFloat_RoundShiftToU64(X, shift)


        If sig53 = TWO53 Then

            sig53 Shr= 1
            binExp += 1

        End If


        If binExp > 1023LL Then

            Return BigFloat_DoubleFromBits( _
                signBits Or DOUBLE_EXP_MASK)

        End If


        Dim expField As ULongInt

        expField = _
            CULngInt(binExp + 1023LL)


        Dim fracField As ULongInt

        fracField = _
            sig53 And DOUBLE_FRAC_MASK


        Dim bits As ULongInt

        bits = _
              signBits _
            Or (expField Shl 52) _
            Or fracField


        Return BigFloat_DoubleFromBits(bits)

    End If


    Dim subShift As LongInt

    subShift = -CLngInt(E) - 1074LL


    Dim subSig As ULongInt

    subSig = BigFloat_RoundShiftToU64(X, subShift)


    If subSig >= TWO52 Then

        Dim bits As ULongInt

        bits = _
              signBits _
            Or &H0010000000000000ULL

        Return BigFloat_DoubleFromBits(bits)

    End If


    If subSig = 0 Then

        Return BigFloat_DoubleFromBits(signBits)

    End If


    Dim bits As ULongInt

    bits = _
          signBits _
        Or subSig

    Return BigFloat_DoubleFromBits(bits)

End Function

' ==============================================================
' BigFloat_Pow2
' ==============================================================
Sub BigFloat_Pow2(ByRef Result As BigFloat, ByVal n As Long)
    BigFloat_Clear Result
    Result.aSig(0) = &H80000000UL
    Result.exponent = n - (SIG_BITS - 1)
End Sub

Sub BigFloat_Sqrt(byref result as BigFloat, byref z as BigFloat)
    dim as long ex, e, prec, max=clng((SIG_LEN * 9.632959861247398))
    dim as BigFloat x
    dim as BigFloat m, tmp, r, r2
    dim as double dx

    If IsNaN_F(z) Then
        BigFloat_SetNaN result
        exit sub
    End If

    If IsZero_F(z) Then
        BigFloat_Clear result
        exit sub
    End If

    If IsNeg_F(z) Then
        BigFloat_SetNaN result
        exit Sub
    End If

    BigFloat_Frexp(z, m, ex)
    if ex and 1 then
        BigFloat_Add m, m, m
        ex=ex-1
    end if
    e=ex shr 1 'ex\2
    dx = BigFloat_ToDouble(m)
    dx = sqr(dx)

    BigFloat_FromDouble r, dx
	prec=32
    do
        BigFloat_Divide tmp, m, r
        BigFloat_Add r2, r, tmp
        BigFloat_Div2 r, r2
        if prec>max then exit do
        prec*=2
    loop

    BigFloat_Pow2 r2, e
    BigFloat_Multiply result, r, r2
end Sub

' oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
Operator Abs(Byref rhs As BigFloat_t) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_Abs Result.BigNum, rhs.BigNum
	Operator = result
End Operator

Operator  Fix (Byref x As BigFloat_t) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_Truncate Result.BigNum, x.BigNum
	Operator = result
End Operator

Operator Frac(Byref x As BigFloat_t) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_Truncate Result.BigNum, x.BigNum
	BigFloat_Subtract Result.BigNum, x.BigNum, Result.BigNum
	Operator = result
End Operator

Operator Sqr(Byref x As BigFloat_t) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_Sqrt result.BigNum, x.BigNum
	Return result
End Operator

Constructor BigFloat_t ( )
	BigFloat_FromLong this.BigNum, 0
End Constructor

Destructor BigFloat_t ( )
End Destructor

Constructor BigFloat_t ( Byval rhs As Long )
	BigFloat_FromLong this.BigNum, rhs
End Constructor

Constructor BigFloat_t ( Byval rhs As Longint )
	BigFloat_FromLongInt this.BigNum, rhs
End Constructor

Constructor BigFloat_t ( Byval rhs As uLong )
	BigFloat_FromULong this.BigNum, rhs
End Constructor

Constructor BigFloat_t ( Byval rhs As uLongint )
	BigFloat_FromULongInt this.BigNum, rhs
End Constructor

Constructor BigFloat_t ( Byval rhs As Double )
	BigFloat_FromDouble this.BigNum, rhs
End Constructor

Constructor BigFloat_t ( Byref rhs As String )
	BigFloat_FromString this.BigNum, rhs
End Constructor

Constructor BigFloat_t ( Byref rhs As BigFloat_t )
	BigFloat_Assign this.BigNum, rhs.BigNum
End Constructor

Operator BigFloat_t.let ( Byval rhs As Long )
	BigFloat_FromLong this.BigNum, rhs
End Operator

Operator BigFloat_t.let ( Byval rhs As LongInt )
	BigFloat_FromLongInt this.BigNum, rhs
End Operator

Operator BigFloat_t.let ( Byval rhs As ULong )
	BigFloat_FromULong this.BigNum, rhs
End Operator

Operator BigFloat_t.let ( Byval rhs As ULongInt )
	BigFloat_FromULongInt this.BigNum, rhs
End Operator

Operator BigFloat_t.let ( Byval rhs As Double )
	BigFloat_FromDouble this.BigNum, rhs
End Operator

Operator BigFloat_t.let ( Byref rhs As String )
	BigFloat_FromString this.BigNum, rhs
End Operator

Operator BigFloat_t.let ( Byref rhs As BigFloat_t )
	BigFloat_Assign this.BigNum, rhs.BigNum
End Operator

Operator BigFloat_t.cast ( ) As Long
	dim as long x
	BigFloat_ToLong this.BigNum, x
	Operator = x
End Operator

Operator BigFloat_t.cast ( ) As LongInt
	dim as LongInt x
	BigFloat_ToLongInt this.BigNum, x
	Operator = x
End Operator

Operator BigFloat_t.cast ( ) As Double
	Operator = BigFloat_ToDouble(this.BigNum)
End Operator

Operator BigFloat_t.cast ( ) As String
	Operator = BigFloat_ToString(this.BigNum)
End Operator

Operator + ( Byref lhs As BigFloat_t, Byval rhs As BigFloat_t ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_Add result.BigNum, lhs.BigNum, rhs.BigNum
	Operator = result
End Operator

Operator + ( Byref lhs As BigFloat_t, Byval rhs As Double ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromDouble result.BigNum, rhs
	BigFloat_Add result.BigNum, lhs.BigNum, result.BigNum
	Operator = result
End Operator

Operator + ( Byval lhs As Double, Byref rhs As BigFloat_t ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromDouble result.BigNum, lhs
	BigFloat_Add result.BigNum, result.BigNum, rhs.BigNum
	Operator = result
End Operator

Operator + ( Byref lhs As BigFloat_t, Byval rhs As Longint ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromLongInt result.BigNum, rhs
	BigFloat_Add result.BigNum, lhs.BigNum, result.BigNum
	Operator = result
End Operator

Operator + ( Byval lhs As Longint, byref rhs As BigFloat_t  ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromLongInt result.BigNum, lhs
	BigFloat_Add result.BigNum, result.BigNum, rhs.BigNum
	Operator = result
End Operator

Operator + (Byref lhs As BigFloat_t, byref rhs as String) As BigFloat_t
	dim as BigFloat_t result
	BigFloat_FromString result.BigNum, rhs
	BigFloat_Add result.BigNum, lhs.BigNum, result.BigNum
	Operator = result
End Operator

Operator - ( Byref rhs As BigFloat_t ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_Negate result.BigNum, rhs.BigNum
	Operator = result
End Operator

Operator - ( Byref lhs As BigFloat_t, Byval rhs As BigFloat_t ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_Subtract result.BigNum, lhs.BigNum, rhs.BigNum
	Operator = result
End Operator

Operator - ( Byref lhs As BigFloat_t, Byval rhs As Double ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromDouble result.BigNum, rhs
	BigFloat_Subtract result.BigNum, lhs.BigNum, result.BigNum
	Operator = result
End Operator

Operator - ( Byval lhs As Double, Byref rhs As BigFloat_t ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromDouble result.BigNum, lhs
	BigFloat_Subtract result.BigNum, result.BigNum, rhs.BigNum
	Operator = result
End Operator

Operator - ( Byref lhs As BigFloat_t, Byval rhs As Longint ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromLongInt result.BigNum, rhs
	BigFloat_Subtract result.BigNum, lhs.BigNum, result.BigNum
	Operator = result
End Operator

Operator - ( Byval lhs As Longint, byref rhs As BigFloat_t  ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromLongInt result.BigNum, lhs
	BigFloat_Subtract result.BigNum, result.BigNum, rhs.BigNum
	Operator = result
End Operator

Operator - (Byref lhs As BigFloat_t, byref rhs as String) As BigFloat_t
	dim as BigFloat_t result
	BigFloat_FromString result.BigNum, rhs
	BigFloat_Subtract result.BigNum, lhs.BigNum, result.BigNum
	Operator = result
End Operator


Operator * ( Byref lhs As BigFloat_t, Byval rhs As BigFloat_t ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_Multiply result.BigNum, lhs.BigNum, rhs.BigNum
	Operator = result
End Operator

Operator * ( Byref lhs As BigFloat_t, Byval rhs As Double ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromDouble result.BigNum, rhs
	BigFloat_Multiply result.BigNum, lhs.BigNum, result.BigNum
	Operator = result
End Operator

Operator * ( Byval lhs As Double, Byref rhs As BigFloat_t ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromDouble result.BigNum, lhs
	BigFloat_Multiply result.BigNum, result.BigNum, rhs.BigNum
	Operator = result
End Operator

Operator * ( Byref lhs As BigFloat_t, Byval rhs As Longint ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromLongInt result.BigNum, rhs
	BigFloat_Multiply result.BigNum, lhs.BigNum, result.BigNum
	Operator = result
End Operator

Operator * ( Byval lhs As Longint, byref rhs As BigFloat_t  ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromLongInt result.BigNum, lhs
	BigFloat_Multiply result.BigNum, result.BigNum, rhs.BigNum
	Operator = result
End Operator

Operator * (Byref lhs As BigFloat_t, byref rhs as String) As BigFloat_t
	dim as BigFloat_t result
	BigFloat_FromString result.BigNum, rhs
	BigFloat_Multiply result.BigNum, lhs.BigNum, result.BigNum
	Operator = result
End Operator


Operator / ( Byref lhs As BigFloat_t, Byval rhs As BigFloat_t ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_Divide result.BigNum, lhs.BigNum, rhs.BigNum
	Operator = result
End Operator

Operator / ( Byref lhs As BigFloat_t, Byval rhs As Double ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromDouble result.BigNum, rhs
	BigFloat_Divide result.BigNum, lhs.BigNum, result.BigNum
	Operator = result
End Operator

Operator / ( Byval lhs As Double, Byref rhs As BigFloat_t ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromDouble result.BigNum, lhs
	BigFloat_Divide result.BigNum, result.BigNum, rhs.BigNum
	Operator = result
End Operator

Operator / ( Byref lhs As BigFloat_t, Byval rhs As Longint ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromLongInt result.BigNum, rhs
	BigFloat_Divide result.BigNum, lhs.BigNum, result.BigNum
	Operator = result
End Operator

Operator / ( Byval lhs As Longint, byref rhs As BigFloat_t  ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromLongInt result.BigNum, lhs
	BigFloat_Divide result.BigNum, result.BigNum, rhs.BigNum
	Operator = result
End Operator

Operator / ( Byref lhs As BigFloat_t, Byval rhs As Long ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromLong result.BigNum, rhs
	BigFloat_Divide result.BigNum, lhs.BigNum, result.BigNum
	Operator = result
End Operator

Operator / ( Byval lhs As Long, byref rhs As BigFloat_t  ) As BigFloat_t
	Dim As BigFloat_t result
	BigFloat_FromLong result.BigNum, lhs
	BigFloat_Divide result.BigNum, result.BigNum, rhs.BigNum
	Operator = result
End Operator

Operator / (Byref lhs As BigFloat_t, byref rhs as String) As BigFloat_t
	dim as BigFloat_t result
	BigFloat_FromString result.BigNum, rhs
	BigFloat_Divide result.BigNum, lhs.BigNum, result.BigNum
	Operator = result
End Operator

' ==============================================================

Operator = ( Byref lhs As BigFloat_t, Byref rhs As BigFloat_t ) As Long
	Operator = BigFloat_Compare(lhs.BigNum, rhs.BigNum)=0
End Operator

Operator < ( Byref lhs As BigFloat_t, Byref rhs As BigFloat_t ) As Long
	Operator = BigFloat_Compare(lhs.BigNum, rhs.BigNum)<0
End Operator

Operator > ( Byref lhs As BigFloat_t, Byref rhs As BigFloat_t ) As Long
	Operator = BigFloat_Compare(lhs.BigNum, rhs.BigNum)>0
End Operator

Operator <= ( Byref lhs As BigFloat_t, Byref rhs As BigFloat_t ) As Long
	Operator = BigFloat_Compare(lhs.BigNum, rhs.BigNum)<=0
End Operator

Operator >= ( Byref lhs As BigFloat_t, Byref rhs As BigFloat_t ) As Long
	Operator = BigFloat_Compare(lhs.BigNum, rhs.BigNum)>=0
End Operator

Operator <> ( Byref lhs As BigFloat_t, Byref rhs As BigFloat_t ) As Long
	Operator = BigFloat_Compare(lhs.BigNum, rhs.BigNum)<>0
End Operator

' ==============================================================

function fpipow(byref x as BigFloat_t, byval e as longint) as BigFloat_t
	dim as BigFloat_t one, y, r
	dim as longint n

	'take x to an long power

	y = x
	if (e < 0) then
		n = -e
	else
		n = e
	end if

	BigFloat_FromLong r.BigNum, 1

	one = r
	while (n > 0)
		while ((n and 1) = 0)
			n=n shr 1
			y=y*y
		wend
		n -= 1
		r=y*r
	wend
	if (e < 0) then	r=one/r
	return r
end function

sub binarysplittingRamanujanPI0(byval a as const ulongint, byval b as const ulongint, byref p as BigFloat, byref q as BigFloat, byref r as BigFloat)
	dim as BigFloat pp, qq, rr, tmp
	dim as ulongint md

	if (b - a) = 1 then
		if b <= 832256 then
                        BigFloat_FromULongInt r, ((2 * b) - 1)
                        BigFloat_FromULongInt tmp, ((4 * b) - 3)
                        BigFloat_Multiply r, r, tmp
                        BigFloat_FromULongInt tmp, ((4 * b) - 1)
                        BigFloat_Multiply r, r, tmp
		else
			BigFloat_FromULongInt r, (((4 * b) - 3) * ((4 * b) - 1))
			BigFloat_FromULongInt tmp, ((2 * b) - 1)
			BigFloat_Multiply r, r, tmp
		end if
		BigFloat_FromULongInt p, (1103ull + (26390ull * b))
		BigFloat_Multiply p, p, r
		BigFloat_FromULongInt q, b
		BigFloat_Multiply tmp, q, q
		BigFloat_Multiply q, q, tmp
		BigFloat_FromULongInt tmp, 3073907232ull
		BigFloat_Multiply q, q, tmp
		return
	end if
	md = (a + b) \ 2
	binarysplittingRamanujanPI0 a, md, p, q, r
	binarysplittingRamanujanPI0 md, b, pp, qq, rr
	BigFloat_Multiply tmp, p, qq
	BigFloat_Multiply p, pp, r
	BigFloat_Add p, p, tmp
	BigFloat_Multiply q, q, qq
	BigFloat_Multiply r, r, rr
end sub

sub Pi_Ramanujan0(byref result as BigFloat, byval digits as ulongint=(SIG_LEN * 9.632959861247398))
	dim as ulongint k
	dim as BigFloat p, q, r
	dim as BigFloat pi1, tmp
	k = cast(ulongint,-int(-( digits*log(10) / log(96059301ull) )))
	binarysplittingRamanujanPI0(0, k, p, q, r)
	BigFloat_FromULongInt pi1, 9801ull
	BigFloat_Multiply pi1, pi1, q
	BigFloat_FromULongInt tmp, 1103ull
	BigFloat_Multiply tmp, tmp, q
	BigFloat_Add tmp, p, tmp
	BigFloat_Divide pi1, pi1, tmp
	BigFloat_FromULongInt tmp, 8ull
	BigFloat_Sqrt tmp, tmp
	BigFloat_Divide result, pi1, tmp
end sub

sub binarysplittingRamanujanPI(byval a as const ulongint, byval b as const ulongint, byref p as BigFloat, byref q as BigFloat, byref r as BigFloat)
	dim as BigFloat pp, qq, rr, tmp
	dim as ulongint md

	if (b - a) = 1 then
		if b <= 832256 then
                        BigFloat_FromULongInt r, ((2 * b) - 1)
                        BigFloat_MultiplyU64 r, r, ((4 * b) - 3)
                        BigFloat_MultiplyU64 r, r, ((4 * b) - 1)
		else
			BigFloat_FromULongInt r, (((4 * b) - 3) * ((4 * b) - 1))
			BigFloat_MultiplyU64 r, r, ((2 * b) - 1)
		end if
		BigFloat_FromULongInt p, (1103ull + (26390ull * b))
		BigFloat_Multiply p, p, r
		BigFloat_FromULongInt q, b
		BigFloat_Multiply tmp, q, q
		BigFloat_Multiply q, q, tmp
		BigFloat_MultiplyU64 q, q, 3073907232ull
		return
	end if
	md = (a + b) \ 2
	binarysplittingRamanujanPI a, md, p, q, r
	binarysplittingRamanujanPI md, b, pp, qq, rr
	BigFloat_Multiply tmp, p, qq
	BigFloat_Multiply p, pp, r
	BigFloat_Add p, p, tmp
	BigFloat_Multiply q, q, qq
	BigFloat_Multiply r, r, rr
end sub

sub Pi_Ramanujan(byref result as BigFloat, byval digits as ulongint=(SIG_LEN * 9.632959861247398))
	dim as ulongint k
	dim as BigFloat p, q, r
	dim as BigFloat pi1, tmp
	k = cast(ulongint,-int(-( digits*log(10) / log(96059301ull) )))
	binarysplittingRamanujanPI(0, k, p, q, r)
	BigFloat_MultiplyU64 pi1, q, 9801ull
	BigFloat_MultiplyU64 tmp, q, 1103ull
	BigFloat_Add tmp, p, tmp
	BigFloat_Divide pi1, pi1, tmp
	BigFloat_FromULongInt tmp, 8ull
	BigFloat_Sqrt tmp, tmp
	BigFloat_Divide result, pi1, tmp
end sub

function agm(byref a_in as BigFloat_t, byref b_in as BigFloat_t) as BigFloat_t
	dim as BigFloat_t a, b, c, d, eps

	a=a_in
	b=b_in
	eps="1e-"+trim(str((SIG_LEN * 9.632959861247398)-4))
	do
		c = (a + b)/2
		d = sqr(a*b)
		a = c
		b = d
	loop until Abs(a-b)<eps
	return a
end function

Sub calcln2()
	dim  As BigFloat_t one
	BigFloat_FromLong one.BigNum, 1
	BigFloat_FromLong fpln2.BigNum, 2
	fpln2 = fpipow(fpln2, (-2*clng((SIG_LEN * 9.632959861247398)+4) + 1))
	fpln2 = agm(one, fpln2)
	fpln2 = (fppi/(2*fpln2))/(2*clng((SIG_LEN * 9.632959861247398)+4) + 1)
End Sub


sub init_constants
	Pi_Ramanujan fppi.BigNum
	calcln2
	BigFloat_Mul2 fppi2.BigNum, fppi.BigNum
	BigFloat_Div2 fpPihalf.BigNum, fppi.BigNum
	BigFloat_MultiplyU64 Pi3o2.BigNum, fppi.BigNum, 3
	BigFloat_Div2 Pi3o2.BigNum, Pi3o2.BigNum
	SIN_TAYLOR_N = -int(-(11.09035488895913*SIG_LEN/(log(1143.335555562796*SIG_LEN/2.718281828459045)-log(log(1143.335555562796*SIG_LEN/2.718281828459045)))-3/2))
end sub

function log_agm(byref x as BigFloat_t) as BigFloat_t
	dim as BigFloat_t y, one=1
	dim as long digits=clng((SIG_LEN * 9.632959861247398))
	
	if x <= 0 then
		print "can nott take fplog_agm of zero or a negative number"
		return 0
	end if

	if x=1 then return 0

	if x < 1 then
		y=2
		y=fpipow(y, 2 - 2 * digits)
		y=y*x
		y=fppi/(2*agm(one, y))-fpln2 * 2 * digits
		y=-y
	else
	
		y=2
		y=fpipow(y, 2 - 2 * digits)
		y=y/x
		y=fppi/(2*agm(one, y))-fpln2 * 2 * digits
	end if
	return y
end function

'Const SIN_TAYLOR_N As Long = 21   ' error < 256^-52 for |x| <= (pi/2)/81; recompute if your
                                   ' precision or reduction range differs (see note below)

Dim Shared As BigFloat_t TaylorSinC(0 To SIN_TAYLOR_N)
'Dim Shared As BigFloat_t TaylorCosC(0 To SIN_TAYLOR_N)
Dim Shared As Boolean TaylorSinC_Ready = False

Sub InitTaylorSinCoeffs()
    Dim As BigFloat_t c
    c = 1
    TaylorSinC(0) = c
    For i As Long = 1 To SIN_TAYLOR_N
        c = -c / ((2*i) * (2*i + 1))
        TaylorSinC(i) = c
    Next i
    TaylorSinC_Ready = True
End Sub

function fpSin(byref z as BigFloat_t) as BigFloat_t
	dim as BigFloat_t sn, x2, x = z
	dim as long sign

	If Not TaylorSinC_Ready Then InitTaylorSinCoeffs()

	sign = 1
	if x < 0 then
		sign = -1
		x = -x
	end if

	if x > fpPi2 then
		sn = x / fpPi2
		BigFloat_Truncate x2.BigNum, sn.BigNum
		x = sn - x2
		x = x * fpPi2
	end if

	if x > fpPihalf then
		if x < Pi3o2 then
			x = fpPi - x
		else
			x = x - fpPi2
		end if
	end if

	x = x / 81
	x2 = x * x

	sn = TaylorSinC(SIN_TAYLOR_N)
	For i as long = SIN_TAYLOR_N - 1 To 0 Step -1
		sn = sn * x2 + TaylorSinC(i)
	Next i
	sn = x * sn

	if sign < 0 then sn = -sn

	x2 = 3
	sn = sn * (x2 - 4*sn*sn)
	sn = sn * (x2 - 4*sn*sn)
	sn = sn * (x2 - 4*sn*sn)
	sn = sn * (x2 - 4*sn*sn)

	return sn
end function

Const COS_TAYLOR_N As Long = 22   ' error < 256^-52 for |x| <= (pi/2)/81

Dim Shared As BigFloat_t TaylorCosC(0 To COS_TAYLOR_N)
Dim Shared As Boolean TaylorCosC_Ready = False

Sub InitTaylorCosCoeffs()
    Dim As BigFloat_t d
    d = 1
    TaylorCosC(0) = d
    For i As Long = 1 To COS_TAYLOR_N
        d = -d / ((2*i - 1) * (2*i))
        TaylorCosC(i) = d
    Next i
    TaylorCosC_Ready = True
End Sub

function fpCos(byref z as BigFloat_t) as BigFloat_t
	dim as BigFloat_t cs, x2, x = z
	dim as long cossign

	If Not TaylorCosC_Ready Then InitTaylorCosCoeffs()

	x = Abs(x)   ' cos is even -- no input-sign tracking needed at all

	if x > fpPi2 then
		dim as BigFloat_t sn, xi
		sn = x / fpPi2
		BigFloat_Truncate xi.BigNum, sn.BigNum
		x = sn - xi
		x = x * fpPi2
	end if

	cossign = 1
	if x > fpPihalf then
		if x < Pi3o2 then
			x = fpPi - x
			cossign = -1        ' cos(pi-x) = -cos(x) -- sine's reflection identity doesn't carry over
		else
			x = x - fpPi2       ' cos(x-2pi) = cos(x) -- same as sine here, no flip
		end if
	end if

	x = x / 81
	x2 = x * x

	cs = TaylorCosC(COS_TAYLOR_N)

	For i as long = COS_TAYLOR_N - 1 To 0 Step -1
	
		cs = cs * x2 + TaylorCosC(i)

	Next i
	' no "cs = x*cs" here -- cosine's series has no leading x factor

	if cossign < 0 then cs = -cs

	dim as BigFloat_t three = 3
	cs = cs * (4*cs*cs - three)
	cs = cs * (4*cs*cs - three)
	cs = cs * (4*cs*cs - three)
	cs = cs * (4*cs*cs - three)

	return cs
end function

function fpCoss(byref z as BigFloat_t) as BigFloat_t
	dim as BigFloat_t sn, x2, x = fpPihalf-z
	dim as long sign

	If Not TaylorSinC_Ready Then InitTaylorSinCoeffs()

	sign = 1
	if x < 0 then
		sign = -1
		x = -x
	end if

	if x > fpPi2 then
		sn = x / fpPi2
		BigFloat_Truncate x2.BigNum, sn.BigNum
		x = sn - x2
		x = x * fpPi2
	end if

	if x > fpPihalf then
		if x < Pi3o2 then
			x = fpPi - x
		else
			x = x - fpPi2
		end if
	end if

	x = x / 81
	x2 = x * x

	sn = TaylorSinC(SIN_TAYLOR_N)
	For i as long = SIN_TAYLOR_N - 1 To 0 Step -1
		sn = sn * x2 + TaylorSinC(i)
	Next i
	sn = x * sn

	if sign < 0 then sn = -sn

	x2 = 3
	sn = sn * (x2 - 4*sn*sn)
	sn = sn * (x2 - 4*sn*sn)
	sn = sn * (x2 - 4*sn*sn)
	sn = sn * (x2 - 4*sn*sn)

	return sn
end function

' ==============================================================
#if 0
dim as BigFloat_t x, y, z
dim as long i
/'
x=123456789
y=fpsin(x)
z=fpcos(x)
print y
print z
'/

'x="3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196442881097566593344612847564823378678316527120190914565"
'for i=0 to SIG_LEN - 1
'	? hex(x.BigNum.aSig(i), 8);", ";
'next
'?
'? fppi
y=sqr(fppi)
? y
#endif


dim as BigFloat A, B, C
dim as BigFloat_t e, x, y, z, s
dim as double t
dim as long i, j, ex
#if 1
/'
BigFloat_FromString A, "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938"
? "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938"
'? BigFloat_ToString(A)
BigFloat_Sqrt C, A
? BigFloat_ToString(C)
'/

t=timer
BigFloat_FromLong C, 0
for j=1 to 10'00
    for i=1 to 10'00
        BigFloat_FromLong A, i
        BigFloat_Sqrt B, A
        BigFloat_Add C, C, B
    next
next
t=timer-t
Print BigFloat_ToString(C)
? t
#endif
