' ============================================================
' BigFloat.bas - binary floating-point engine for FreeBASIC
' Canonical finite nonzero invariant: bit 31 of aSig(0) is set.
' value = (-1)^sign * significand * 2^exponent
' Round mode: nearest, ties to even.
' ============================================================

Type mylong As Long

' 13 limbs x 32 bits = 416 bits.
Const SIG_LEN   As mylong = 1039 '14
Const SIG_BITS  As mylong = 32 * SIG_LEN
Const SIG_BYTES As mylong = 4 * SIG_LEN   ' byte helpers / decimal conversion only

Dim Shared As Long SIN_TAYLOR_N

Type BigFloat
    sign As Long        ' 0 = non-negative, 1 = negative
    bIsNaN As Long      ' 0 = normal value, 1 = NaN
    exponent As Long    ' units of BITS (base 2); value = significand * 2^exponent
    aSig(0 To SIG_LEN - 1) As Ulong
End Type

Type BigFloat_t
    Declare Constructor ( )
    Declare Constructor ( Byval rhs As Long )
    Declare Constructor ( Byval rhs As Longint )
    Declare Constructor ( Byval rhs As Ulong )
    Declare Constructor ( Byval rhs As Ulongint )
    Declare Constructor ( Byval rhs As Double )
    Declare Constructor ( Byref rhs As String )
    Declare Constructor ( Byref rhs As BigFloat_t )

    Declare Destructor ( )

    Declare Operator Let ( Byval rhs As Long )
    Declare Operator Let ( Byval rhs As Longint )
    Declare Operator Let ( Byval rhs As Ulong )
    Declare Operator Let ( Byval rhs As Ulongint )
    Declare Operator Let ( Byval rhs As Double )
    Declare Operator Let ( Byref rhs As String )
    Declare Operator Let ( Byref rhs As BigFloat_t )

    Declare Operator Cast ( ) As Long
    Declare Operator Cast ( ) As Longint
    Declare Operator Cast ( ) As Double
    Declare Operator Cast ( ) As String

    BigNum As BigFloat
End Type

Declare Sub init_constants
Dim Shared As BigFloat_t fppi, fppi2, fpPihalf, Pi3o2, fpln2

init_constants
' ==============================================================
' Forward declarations
' ==============================================================
Declare Sub      BigFloat_Add       (Byref Result As BigFloat, Byref A As BigFloat, Byref B As BigFloat)
Declare Sub      BigFloat_Subtract  (Byref Result As BigFloat, Byref A As BigFloat, Byref B As BigFloat)
Declare Sub      BigFloat_Multiply  (Byref Result As BigFloat, Byref A As BigFloat, Byref B As BigFloat)
Declare Sub      BigFloat_Divide    (Byref Result As BigFloat, Byref A As BigFloat, Byref B As BigFloat)
Declare Sub      BigFloat_FromString(Byref Result As BigFloat, Byval sInput As String)
Declare Function BigFloat_Compare   (Byref A As BigFloat, Byref B As BigFloat) As mylong
Declare Sub      BigFloat_Clear     (Byref F As BigFloat)
Declare Sub      BigFloat_Assign    (Byref Dst As BigFloat, Byref Src As BigFloat)
Declare Sub      BigFloat_SetNaN    (Byref F As BigFloat)
Declare Function BigFloat_ToString  (Byref F As BigFloat) As String
Declare Function IsExpNeg       (Byref F As BigFloat) As Boolean

' Private helpers
Declare Function GetExp   (Byref F As BigFloat) As Long
Declare Sub      SetExp   (Byref F As BigFloat, Byval lE As Long)
Declare Function IsNaN_F  (Byref F As BigFloat) As Boolean
Declare Function IsNeg_F  (Byref F As BigFloat) As Boolean
Declare Function IsZero_F (Byref F As BigFloat) As Boolean
Declare Sub      Normalize (Byref F As BigFloat)
Declare Function SigGetByte(Byref F As BigFloat, Byval iByte As Long) As Ubyte
Declare Sub      SigSetByte(Byref F As BigFloat, Byval iByte As Long, Byval v As Ubyte)
Declare Function BigFloat_GetSigBit(Byref X As BigFloat, _
                                Byval bitIndex As Long) As Ulong
                                
' Native base-2^32 limb helpers
Declare Sub      SigToLE(Byref F As BigFloat, A() As Ulong)
Declare Sub      SigFromLE(Byref F As BigFloat, A() As Ulong)
Declare Function SignificantBytesLE(A() As Ulong) As Long
Declare Function GetByteLE(A() As Ulong, Byval byteIndex As Long) As Ubyte
Declare Function AnyLowBytesNonZeroLE(A() As Ulong, Byval byteCount As Long) As Boolean
Declare Sub      ShiftLeftBytesLE(Src() As Ulong, Dst() As Ulong, Byval shiftBytes As Long)
Declare Sub      ShiftRightBytesTo7LE(Src() As Ulong, Dst() As Ulong, Byval shiftBytes As Long)
Declare Sub      RoundExactLimbs(Byref Result As BigFloat, A() As Ulong, Byval lBaseExp As Long, Byval blExternalSticky As Boolean)
Declare Function LeadingZeroBits32(Byval x As Ulong) As Long
Declare Sub      Div224x224(Byref A As BigFloat, Byref B As BigFloat, Q() As Ulong, R() As Ulong)
Declare Function CompareTwiceRemainder32(R() As Ulong, D() As Ulong) As Long

Declare Sub LimbMulAccumulate(X() As Ulong, Byval nx As Long, Y() As Ulong, Byval ny As Long, P() As Ulong)

Declare Sub SigAdd(Byref Result As BigFloat, Byref A As BigFloat, Byref B As BigFloat, _
                   Byval lExpA As Long, Byval lExpB As Long)
Declare Sub SigSub(Byref Result As BigFloat, Byref A As BigFloat, Byref B As BigFloat, _
                   Byval lExpA As Long, Byval lExpB As Long)
Declare Sub RoundExactBytes(Byref Result As BigFloat, A() As Long, Byval lBaseExp As Long, _
                            Byval blExternalSticky As Boolean)
Declare Sub BigByte_MulSmall(A() As Long, Byval m As Long)
Declare Sub BigByte_AddSmall(A() As Long, Byval v As Long)
Declare Function BigByte_DivSmall(A() As Long, Byval d As Long) As Long

Declare Sub SigRoundIncrement(Byref F As BigFloat, Byref lExp As Long)

Declare Sub      BigDec_Init    (A() As mylong)
Declare Sub      BigDec_TrimMSZ (A() As mylong)
Declare Sub      BigDec_MulSmall(A() As mylong, Byval m As mylong)
Declare Sub      BigDec_AddSmall(A() As mylong, Byval addVal As mylong)
Declare Function BigDec_ToString(A() As mylong) As String

Declare Sub BigFloat_FromULong   (Byref R As BigFloat, Byval v As Ulong)
Declare Sub BigFloat_FromLong    (Byref R As BigFloat, Byval v As Long)
Declare Sub BigFloat_FromULongInt(Byref R As BigFloat, Byval v As Ulongint)
Declare Sub BigFloat_FromLongInt (Byref R As BigFloat, Byval v As Longint)
Declare Sub BigFloat_FromDouble  (Byref R As BigFloat, Byval v As Double)

Declare Function BigFloat_ToULong   (Byref X As BigFloat, Byref v As Ulong) As Boolean
Declare Function BigFloat_ToLong    (Byref X As BigFloat, Byref v As Long) As Boolean
Declare Function BigFloat_ToULongInt(Byref X As BigFloat, Byref v As Ulongint) As Boolean
Declare Function BigFloat_ToLongInt (Byref X As BigFloat, Byref v As Longint) As Boolean
Declare Function BigFloat_ToDouble  (Byref X As BigFloat) As Double

Declare Sub BigFloat_Frexp(Byref X As BigFloat, _
               Byref Mantissa As BigFloat, _
               Byref BinExponent As Long)
Declare Sub BigFloat_Mul2(Byref Result As BigFloat, Byref X As BigFloat)
Declare Sub BigFloat_Div2(Byref Result As BigFloat, Byref X As BigFloat)
Declare Sub BigFloat_Abs(Byref Result As BigFloat, Byref X As BigFloat)
Declare Sub BigFloat_Negate(Byref Result As BigFloat, Byref X As BigFloat)

' Arithmetic against a plain 64-bit integer scalar.  Add/Subtract/
' Divide are correctness-first wrappers built on BigFloat_FromLongInt /
' BigFloat_FromULongInt plus the existing general routines (building the
' scalar operand is O(1), so this costs almost nothing extra).
' Multiply gets a genuinely dedicated fast path -- see
' BigFloat_MultiplyU64 -- since a 224x64 product is much cheaper than a
' full 224x224 one and this is a common enough operation to be worth
' the dedicated code.
Declare Sub BigFloat_AddI64      (Byref Result As BigFloat, Byref X As BigFloat, Byval v As Longint)
Declare Sub BigFloat_AddU64      (Byref Result As BigFloat, Byref X As BigFloat, Byval v As Ulongint)
Declare Sub BigFloat_SubtractI64 (Byref Result As BigFloat, Byref X As BigFloat, Byval v As Longint)
Declare Sub BigFloat_SubtractU64 (Byref Result As BigFloat, Byref X As BigFloat, Byval v As Ulongint)
Declare Sub BigFloat_MultiplyI64 (Byref Result As BigFloat, Byref X As BigFloat, Byval v As Longint)
Declare Sub BigFloat_MultiplyU64 (Byref Result As BigFloat, Byref X As BigFloat, Byval mag As Ulongint)
Declare Sub BigFloat_DivideI64   (Byref Result As BigFloat, Byref X As BigFloat, Byval v As Longint)
Declare Sub BigFloat_DivideU64   (Byref Result As BigFloat, Byref X As BigFloat, Byval v As Ulongint)

' ==============================================================
' ADD
' Round-to-nearest, ties-to-even.
' ==============================================================
Sub BigFloat_Add(Byref Result As BigFloat, Byref A As BigFloat, Byref B As BigFloat)
    If IsNaN_F(A) Orelse IsNaN_F(B) Then
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

        If blNegA Andalso Not IsZero_F(Result) Then
            Result.sign = 1
        End If

    Else
        Dim blABigger As Boolean
        Dim iSigIdx As mylong

        If lExpA > lExpB Then
            blABigger = True
        Elseif lExpA < lExpB Then
            blABigger = False
        Else
            blABigger = True
            For iSigIdx = 0 To SIG_LEN - 1
                If AA.aSig(iSigIdx) > BB.aSig(iSigIdx) Then
                    blABigger = True
                    Exit For
                Elseif AA.aSig(iSigIdx) < BB.aSig(iSigIdx) Then
                    blABigger = False
                    Exit For
                End If
            Next
        End If

        If blABigger Then
            SigSub Result, AA, BB, lExpA, lExpB
            If blNegA Andalso Not IsZero_F(Result) Then Result.sign = 1
        Else
            SigSub Result, BB, AA, lExpB, lExpA
            If blNegB Andalso Not IsZero_F(Result) Then Result.sign = 1
        End If
    End If
End Sub

' ==============================================================
' SUBTRACT
' ==============================================================
Sub BigFloat_Subtract(Byref Result As BigFloat, Byref A As BigFloat, Byref B As BigFloat)
    If IsNaN_F(A) Orelse IsNaN_F(B) Then
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
' Adaptive 1/2-limb, sparse and dense native base-2^32 product.
' This is safe when Result aliases A or B: operands are read first.
' ==============================================================
Sub BigFloat_Multiply(Byref Result As BigFloat, Byref A As BigFloat, Byref B As BigFloat)
    If IsNaN_F(A) Orelse IsNaN_F(B) Then
        BigFloat_SetNaN Result
        Exit Sub
    End If
    If IsZero_F(A) Orelse IsZero_F(B) Then
        BigFloat_Clear Result
        Exit Sub
    End If

    Dim blNegResult As Boolean = (IsNeg_F(A) Xor IsNeg_F(B))
    Dim lBaseExp As Long = GetExp(A) + GetExp(B)

    Dim AL(0 To SIG_LEN - 1) As ULong
    Dim BL(0 To SIG_LEN - 1) As ULong
    Dim idxA(0 To SIG_LEN - 1) As Long
    Dim idxB(0 To SIG_LEN - 1) As Long
    Dim nA As Long = 0, nB As Long = 0
    Dim i As Long, j As Long, k As Long
    Dim P(0 To 2 * SIG_LEN) As ULong
    Dim carry As ULongInt, t As ULongInt

    For i = 0 To SIG_LEN - 1
        AL(i) = A.aSig(SIG_LEN - 1 - i)
        BL(i) = B.aSig(SIG_LEN - 1 - i)
        If AL(i) <> 0 Then
            idxA(nA) = i
            nA += 1
        End If
        If BL(i) <> 0 Then
            idxB(nB) = i
            nB += 1
        End If
    Next

    If nA = 0 OrElse nB = 0 Then
        BigFloat_Clear Result
        Exit Sub
    End If

    ' Fast path: when either significand has at most two nonzero
    ' limbs, accumulate one or two complete multiplication rows.
    ' Swapping the operand buffers locally is safe because multiplication
    ' is commutative and the original operands have already been copied.
    If nA <= 2 OrElse nB <= 2 Then
        Dim shortL(0 To SIG_LEN - 1) As ULong
        Dim longL(0 To SIG_LEN - 1) As ULong
        Dim shortIdx(0 To 1) As Long
        Dim shortCount As Long
        Dim longStart As Long

        If nA <= nB Then
            shortCount = nA
            longStart = idxB(0)
            For i = 0 To SIG_LEN - 1
                shortL(i) = AL(i)
                longL(i) = BL(i)
            Next
            For i = 0 To shortCount - 1
                shortIdx(i) = idxA(i)
            Next
        Else
            shortCount = nB
            longStart = idxA(0)
            For i = 0 To SIG_LEN - 1
                shortL(i) = BL(i)
                longL(i) = AL(i)
            Next
            For i = 0 To shortCount - 1
                shortIdx(i) = idxB(i)
            Next
        End If

        For i = 0 To shortCount - 1
            Dim rowIndex As Long = shortIdx(i)
            Dim factor As ULongInt = CULngInt(shortL(rowIndex))
            carry = 0
            For j = longStart To SIG_LEN - 1
                k = rowIndex + j
                t = CULngInt(P(k)) + factor * CULngInt(longL(j)) + carry
                P(k) = CULng(t And &HFFFFFFFFULL)
                carry = t Shr 32
            Next
            k = rowIndex + SIG_LEN
            Do While carry <> 0
                t = CULngInt(P(k)) + carry
                P(k) = CULng(t And &HFFFFFFFFULL)
                carry = t Shr 32
                k += 1
            Loop
        Next

    ' Sparse path: only nA*nB nonzero 32x32 products.
    ElseIf nA * nB <= SIG_LEN * 3 Then
        Dim product64 As ULongInt
        For i = 0 To nA - 1
            For j = 0 To nB - 1
                k = idxA(i) + idxB(j)
                product64 = CULngInt(AL(idxA(i))) * CULngInt(BL(idxB(j)))
                t = CULngInt(P(k)) + (product64 And &HFFFFFFFFULL)
                P(k) = CULng(t And &HFFFFFFFFULL)
                carry = (product64 Shr 32) + (t Shr 32)
                k += 1
                Do While carry <> 0
                    t = CULngInt(P(k)) + carry
                    P(k) = CULng(t And &HFFFFFFFFULL)
                    carry = t Shr 32
                    k += 1
                Loop
            Next
        Next
    Else
        ' Dense schoolbook: skip leading zero rows, retain full
        ' positional significance for the exact RNE rounder.
        Dim startA As Long = idxA(0)
        Dim startB As Long = idxB(0)
        For i = startA To SIG_LEN - 1
            If AL(i) = 0 Then Continue For
            carry = 0
            For j = startB To SIG_LEN - 1
                k = i + j
                t = CULngInt(P(k)) + _
                    CULngInt(AL(i)) * CULngInt(BL(j)) + carry
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
    End If

    RoundExactLimbs Result, P(), lBaseExp, False
    If blNegResult AndAlso Not IsZero_F(Result) Then Result.sign = 1
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
Sub LimbMulAccumulate(X() As Ulong, Byval nx As Long, Y() As Ulong, Byval ny As Long, P() As Ulong)
    Dim i As Long, j As Long, k As Long
    Dim carry As Ulongint, t As Ulongint

    For i = 0 To nx - 1
        carry = 0

        For j = 0 To ny - 1
            k = i + j
            t = Culngint(P(k)) + Culngint(X(i)) * Culngint(Y(j)) + carry
            P(k) = Culng(t And &HFFFFFFFFULL)
            carry = t Shr 32
        Next

        k = i + ny
        Do While carry <> 0
            t = Culngint(P(k)) + carry
            P(k) = Culng(t And &HFFFFFFFFULL)
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
Sub BigFloat_Divide(Byref Result As BigFloat, Byref A As BigFloat, Byref B As BigFloat)

    If IsNaN_F(A) Orelse IsNaN_F(B) Orelse IsZero_F(B) Then
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
    Dim Q(0 To SIG_LEN) As Ulong        ' 8 quotient limbs (fixed, no heap alloc)
    Dim R(0 To SIG_LEN - 1) As Ulong    ' 7 remainder limbs
    Div224x224 AA, BB, Q(), R()

    Dim lNewExp As Long
    lNewExp = GetExp(A) - GetExp(B) - SIG_BITS

    Dim D(0 To SIG_LEN - 1) As Ulong
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
        Elseif cmpHalf = 0 Then
            If (Result.aSig(SIG_LEN - 1) And 1UL) <> 0 Then
                roundUp = True
            End If
        End If

        If roundUp Then SigRoundIncrement Result, lNewExp
        SetExp Result, lNewExp
    End If

    If blNegResult Andalso Not IsZero_F(Result) Then
        Result.sign = 1
    End If
End Sub

' ==============================================================
' Count leading zero bits in a nonzero 32-bit limb.
' ==============================================================
Function LeadingZeroBits32(Byval x As Ulong) As Long
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
Sub Div224x224(Byref A As BigFloat, Byref B As BigFloat, _
               Q() As Ulong, R() As Ulong)

    Const BASE64 As Ulongint = 4294967296ULL
    Const MASK64 As Ulongint = &HFFFFFFFFULL

    Dim aL(0 To SIG_LEN - 1) As Ulong
    Dim v(0 To SIG_LEN - 1) As Ulong
    Dim i0 As Long
    For i0 = 0 To SIG_LEN - 1
        aL(i0) = A.aSig(SIG_LEN - 1 - i0)
        v(i0)  = B.aSig(SIG_LEN - 1 - i0)
    Next

    ' Q() and R() are now expected to already be sized by the caller
    ' (BigFloat_Divide declares them as fixed arrays) -- no ReDim here.

    Dim s As Long
    s = LeadingZeroBits32(v(SIG_LEN - 1))

    Dim vn(0 To SIG_LEN - 1) As Ulong
    Dim un(0 To 2 * SIG_LEN) As Ulong

    Dim i As Long
    Dim t As Ulongint
    Dim carry As Ulongint

    ' Normalize divisor so bit 31 of its top limb is set.
    carry = 0
    If s = 0 Then
        For i = 0 To SIG_LEN - 1
            vn(i) = v(i)
        Next
    Else
        For i = 0 To SIG_LEN - 1
            t = (Culngint(v(i)) Shl s) Or carry
            vn(i) = Culng(t And MASK64)
            carry = t Shr 32
        Next
    End If

    ' Dividend before normalization is:
    '
    '   [7 zero low limbs][7 limbs of MA]
    '
    ' in little-endian order.
    Dim rawU(0 To 2 * SIG_LEN - 1) As Ulong
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
            t = (Culngint(rawU(i)) Shl s) Or carry
            un(i) = Culng(t And MASK64)
            carry = t Shr 32
        Next
        un(2 * SIG_LEN) = Culng(carry)
    End If

    Dim j As Long
    Dim qhat As Ulongint
    Dim rhat As Ulongint
    Dim num As Ulongint
    Dim lhs As Ulongint
    Dim rhs As Ulongint
    Dim p As Ulongint
    Dim pLow As Ulongint
    Dim borrow As Ulongint
    Dim ui As Ulongint
    Dim negative As Boolean
    Dim sum As Ulongint

    ' n=7, m=7 in Knuth Algorithm D notation.
    For j = SIG_LEN To 0 Step -1

        ' Estimate quotient limb from the top two dividend limbs.
        If un(j + SIG_LEN) = vn(SIG_LEN - 1) Then
            qhat = BASE64 - 1
            rhat = Culngint(un(j + SIG_LEN - 1)) _
                 + Culngint(vn(SIG_LEN - 1))
        Else
            num = (Culngint(un(j + SIG_LEN)) Shl 32) _
                Or Culngint(un(j + SIG_LEN - 1))

            qhat = num \ Culngint(vn(SIG_LEN - 1))
            rhat = num Mod Culngint(vn(SIG_LEN - 1))
        End If

        ' Correct qhat using the next divisor limb.
        Do
            If rhat >= BASE64 Then Exit Do

            lhs = qhat * Culngint(vn(SIG_LEN - 2))
            rhs = (rhat Shl 32) Or Culngint(un(j + SIG_LEN - 2))

            If lhs <= rhs Then Exit Do

            qhat -= 1
            rhat += Culngint(vn(SIG_LEN - 1))
        Loop

        ' un[j..j+7] -= qhat * vn[0..6]
        borrow = 0

        For i = 0 To SIG_LEN - 1
            p = qhat * Culngint(vn(i)) + borrow
            pLow = p And MASK64
            borrow = p Shr 32

            ui = Culngint(un(j + i))

            If ui < pLow Then
                un(j + i) = Culng(BASE64 + ui - pLow)
                borrow += 1
            Else
                un(j + i) = Culng(ui - pLow)
            End If
        Next

        ui = Culngint(un(j + SIG_LEN))

        If ui < borrow Then
            un(j + SIG_LEN) = Culng(BASE64 + ui - borrow)
            negative = True
        Else
            un(j + SIG_LEN) = Culng(ui - borrow)
            negative = False
        End If

        ' qhat was one too large: add divisor back once.
        If negative Then
            qhat -= 1
            carry = 0

            For i = 0 To SIG_LEN - 1
                sum = Culngint(un(j + i)) _
                    + Culngint(vn(i)) _
                    + carry

                un(j + i) = Culng(sum And MASK64)
                carry = sum Shr 32
            Next

            un(j + SIG_LEN) = _
                Culng((Culngint(un(j + SIG_LEN)) + carry) And MASK64)
        End If

        Q(j) = Culng(qhat)
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
Function CompareTwiceRemainder32(R() As Ulong, D() As Ulong) As Long
    Dim T(0 To SIG_LEN) As Ulong
    Dim carry As Ulongint = 0
    Dim x As Ulongint
    Dim i As Long

    For i = 0 To SIG_LEN - 1
        x = Culngint(R(i)) * 2ULL + carry
        T(i) = Culng(x And &HFFFFFFFFULL)
        carry = x Shr 32
    Next

    T(SIG_LEN) = Culng(carry)

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
Sub SigRoundIncrement(Byref F As BigFloat, Byref lExp As Long)
    Dim i As Long
    Dim t As Ulongint
    Dim carry As Ulongint = 1

    For i = SIG_LEN - 1 To 0 Step -1
        t = Culngint(F.aSig(i)) + carry
        F.aSig(i) = Culng(t And &HFFFFFFFFULL)
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
Sub BigFloat_FromString(Byref Result As BigFloat, Byval sInput As String)
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
    Elseif Left(s, 1) = "+" Then
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
    iEpos = Instr(Lcase(s), "e")

    If iEpos > 0 Then
        Dim sExp As String
        sExp = Mid(s, iEpos + 1)

        If Len(sExp) = 0 Then
            BigFloat_SetNaN Result
            Exit Sub
        End If

        lDecExp = Clng(sExp)
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
    iDot = Instr(s, ".")

    If iDot > 0 Then
        ' Reject a second decimal point.
        If Instr(iDot + 1, s, ".") > 0 Then
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

        If digit < 0 Orelse digit > 9 Then
            BigFloat_SetNaN Result
            Exit Sub
        End If
    Next

    ' Remove leading zeroes so the precision estimate below is based
    ' on the actual number of significant decimal digits.
    Do While Len(s) > 1 Andalso Left(s, 1) = "0"
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
    Redim N(0 To 0)
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
                     Clng(Int(Cdbl(deficitDigits) * 0.4152410118609203 + 0.9999999999999999))

        Dim oldUB As Long
        oldUB = Ubound(N)

        Redim Preserve N(0 To oldUB + extraBytes)

        ' ReDim Preserve zero-initializes the appended bytes, so
        ' this is exactly multiplication by 256^extraBytes.

        Dim blExternalSticky As Boolean = False
        Dim Rem10 As Long

        For i = 1 To k
            Rem10 = BigByte_DivSmall(N(), 10)
            If Rem10 <> 0 Then blExternalSticky = True
        Next

        RoundExactBytes Result, N(), -8 * extraBytes, blExternalSticky
    End If

    If blNeg Andalso Not IsZero_F(Result) Then
        Result.sign = 1
    End If
End Sub

' ==============================================================
' COMPARE   -1=A<B, 0=A=B, +1=A>B
' ==============================================================
Function BigFloat_Compare(Byref A As BigFloat, Byref B As BigFloat) As mylong
    If IsNaN_F(A) Orelse IsNaN_F(B) Then Return 0

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
    Elseif lExpA < lExpB Then
        iCmp = -1
    Else
        ' Limb-wise unsigned comparison; aSig(0) is most significant.
        For iSig = 0 To SIG_LEN - 1
            If A.aSig(iSig) > B.aSig(iSig) Then
                iCmp = 1
                Exit For
            Elseif A.aSig(iSig) < B.aSig(iSig) Then
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
Sub BigFloat_Clear(Byref F As BigFloat)
    F.sign = 0
    F.bIsNaN = 0
    F.exponent = 0
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        F.aSig(i) = 0
    Next
End Sub

Sub BigFloat_Assign(Byref Dst As BigFloat, Byref Src As BigFloat)
    Dst.sign = Src.sign
    Dst.bIsNaN = Src.bIsNaN
    Dst.exponent = Src.exponent
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        Dst.aSig(i) = Src.aSig(i)
    Next
End Sub

Sub BigFloat_SetNaN(Byref F As BigFloat)
    BigFloat_Clear F
    F.bIsNaN = 1
End Sub

' ==============================================================
' HEADER HELPERS
' ==============================================================
Function GetExp(Byref F As BigFloat) As Long
    Return F.exponent
End Function

Sub SetExp(Byref F As BigFloat, Byval lE As Long)
    F.exponent = lE
End Sub

Function IsNaN_F(Byref F As BigFloat) As Boolean
    Return (F.bIsNaN <> 0)
End Function

Function IsNeg_F(Byref F As BigFloat) As Boolean
    Return (F.sign <> 0)
End Function

Function IsZero_F(Byref F As BigFloat) As Boolean
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
Function SigGetByte(Byref F As BigFloat, Byval iByte As Long) As Ubyte
    Dim iLimb As Long = iByte Shr 2
    Dim iShift As Long = (3 - (iByte And 3)) Shl 3
    Return Cubyte((F.aSig(iLimb) Shr iShift) And &HFFUL)
End Function

Sub SigSetByte(Byref F As BigFloat, Byval iByte As Long, Byval v As Ubyte)
    Dim iLimb As Long = iByte Shr 2
    Dim iShift As Long = (3 - (iByte And 3)) Shl 3
    Dim mask As Ulong = &HFFUL Shl iShift

    F.aSig(iLimb) = (F.aSig(iLimb) And Not mask) Or (Culng(v) Shl iShift)
End Sub

' ==============================================================
' NORMALIZE
' ==============================================================
Sub Normalize(Byref F As BigFloat)
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
    Dim T(0 To SIG_LEN - 1) As Ulong
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
Sub SigToLE(Byref F As BigFloat, A() As Ulong)
    Redim A(0 To SIG_LEN - 1) As Ulong
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        A(i) = F.aSig(SIG_LEN - 1 - i)
    Next
End Sub

Sub SigFromLE(Byref F As BigFloat, A() As Ulong)
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        If i <= Ubound(A) Then
            F.aSig(SIG_LEN - 1 - i) = A(i)
        Else
            F.aSig(SIG_LEN - 1 - i) = 0
        End If
    Next
End Sub

Function SignificantBytesLE(A() As Ulong) As Long
    Dim h As Long
    h = Ubound(A)

    Do While h >= Lbound(A) Andalso A(h) = 0
        h -= 1
    Loop

    If h < Lbound(A) Then Return 0

    Dim x As Ulong
    x = A(h)

    Dim topBytes As Long
    If (x And &HFF000000UL) <> 0 Then
        topBytes = 4
    Elseif (x And &H00FF0000UL) <> 0 Then
        topBytes = 3
    Elseif (x And &H0000FF00UL) <> 0 Then
        topBytes = 2
    Else
        topBytes = 1
    End If

    Return h * 4 + topBytes
End Function

Function GetByteLE(A() As Ulong, Byval byteIndex As Long) As Ubyte
    If byteIndex < 0 Then Return 0

    Dim limb As Long
    limb = byteIndex Shr 2

    If limb > Ubound(A) Then Return 0

    Dim sh As Long
    sh = (byteIndex And 3) Shl 3

    Return Cubyte((A(limb) Shr sh) And &HFFUL)
End Function

Function AnyLowBytesNonZeroLE(A() As Ulong, Byval byteCount As Long) As Boolean
    If byteCount <= 0 Then Return False

    Dim fullLimbs As Long
    Dim remBytes As Long
    Dim i As Long

    fullLimbs = byteCount Shr 2
    remBytes = byteCount And 3

    For i = 0 To fullLimbs - 1
        If i <= Ubound(A) Then
            If A(i) <> 0 Then Return True
        End If
    Next

    If remBytes <> 0 Andalso fullLimbs <= Ubound(A) Then
        Dim mask As Ulong

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

Sub ShiftLeftBytesLE(Src() As Ulong, Dst() As Ulong, Byval shiftBytes As Long)
    Dim limbShift As Long
    Dim bitShift As Long
    limbShift = shiftBytes Shr 2
    bitShift = (shiftBytes And 3) Shl 3

    Redim Dst(0 To Ubound(Src) + limbShift + 1) As Ulong

    Dim i As Long
    Dim t As Ulongint
    Dim carry As Ulongint = 0

    If bitShift = 0 Then
        For i = 0 To Ubound(Src)
            Dst(i + limbShift) = Src(i)
        Next
        Exit Sub
    End If

    For i = 0 To Ubound(Src)
        t = (Culngint(Src(i)) Shl bitShift) Or carry
        Dst(i + limbShift) = Culng(t And &HFFFFFFFFULL)
        carry = t Shr 32
    Next

    Dst(Ubound(Src) + limbShift + 1) = Culng(carry)
End Sub

Sub ShiftRightBytesTo7LE(Src() As Ulong, Dst() As Ulong, Byval shiftBytes As Long)
    Redim Dst(0 To SIG_LEN - 1) As Ulong

    Dim limbShift As Long
    Dim bitShift As Long
    limbShift = shiftBytes Shr 2
    bitShift = (shiftBytes And 3) Shl 3

    Dim i As Long
    Dim lo As Ulong, hi As Ulong

    For i = 0 To SIG_LEN - 1
        If i + limbShift <= Ubound(Src) Then
            lo = Src(i + limbShift)
        Else
            lo = 0
        End If

        If bitShift = 0 Then
            Dst(i) = lo
        Else
            If i + limbShift + 1 <= Ubound(Src) Then
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
Sub RoundExactLimbs(Byref Result As BigFloat, A() As Ulong, _
                    Byval lBaseExp As Long, _
                    Byval blExternalSticky As Boolean)
    BigFloat_Clear Result

    Dim hi As Long = Ubound(A)
    Do While hi >= Lbound(A) Andalso A(hi) = 0
        hi -= 1
    Loop
    If hi < Lbound(A) Then Exit Sub

    Dim highBit As Long
    highBit = 31 - LeadingZeroBits32(A(hi))

    Dim nBits As Long
    nBits = hi * 32 + highBit + 1

    Dim T(0 To SIG_LEN - 1) As Ulong
    Dim i As Long, src As Long
    Dim lExp As Long

    If nBits <= SIG_BITS Then
        Dim leftShift As Long = SIG_BITS - nBits
        Dim limbShift As Long = leftShift Shr 5
        Dim bitShift As Long = leftShift And 31
        Dim carry As Ulongint = 0
        Dim x As Ulongint

        For i = 0 To hi
            src = i + limbShift
            If src < SIG_LEN Then
                x = (Culngint(A(i)) Shl bitShift) Or carry
                T(src) = Culng(x And &HFFFFFFFFULL)
                carry = x Shr 32
            End If
        Next
        If bitShift <> 0 Then
            src = hi + limbShift + 1
            If src < SIG_LEN Then T(src) = Culng(carry)
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
    Dim lo As Ulong, hh As Ulong

    For i = 0 To SIG_LEN - 1
        src = i + rLimb
        If src <= Ubound(A) Then lo = A(src) Else lo = 0
        If rBits = 0 Then
            T(i) = lo
        Else
            If src + 1 <= Ubound(A) Then hh = A(src + 1) Else hh = 0
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
        If (Not sticky) Andalso remBits <> 0 Then
            Dim mask As Ulong
            mask = (1UL Shl remBits) - 1UL
            If (A(full) And mask) <> 0 Then sticky = True
        End If
    End If

    Dim roundUp As Boolean
    roundUp = guard Andalso (sticky Orelse CBool((T(0) And 1UL) <> 0))
    lExp = lBaseExp + drop

    If roundUp Then
        Dim c As Ulongint = 1
        Dim u As Ulongint
        For i = 0 To SIG_LEN - 1
            u = Culngint(T(i)) + c
            T(i) = Culng(u And &HFFFFFFFFULL)
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
Sub RoundExactBytes(Byref Result As BigFloat, A() As Long, Byval lBaseExp As Long, _
                    Byval blExternalSticky As Boolean)
    Dim first As Long = Lbound(A)
    Dim last As Long = Ubound(A)
    Do While first <= last Andalso A(first) = 0
        first += 1
    Loop
    If first > last Then
        BigFloat_Clear Result
        Exit Sub
    End If

    Dim nBytes As Long = last - first + 1
    Dim nLimbs As Long = (nBytes + 3) Shr 2
    Dim T() As Ulong
    Redim T(0 To nLimbs - 1) As Ulong

    Dim i As Long, position As Long, li As Long, sh As Long
    For i = last To first Step -1
        position = last - i
        li = position Shr 2
        sh = (position And 3) Shl 3
        T(li) Or= Culng(A(i) And 255) Shl sh
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
Sub SigAdd(Byref Result As BigFloat, Byref A As BigFloat, Byref B As BigFloat, _
           Byval lExpA As Long, Byval lExpB As Long)
    Dim d As Long = lExpA - lExpB
    If d > SIG_BITS Then
        BigFloat_Assign Result, A
        Result.sign = 0
        Exit Sub
    End If

    Dim AL(0 To SIG_LEN - 1) As Ulong
    Dim BL(0 To SIG_LEN - 1) As Ulong
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        AL(i) = A.aSig(SIG_LEN - 1 - i)
        BL(i) = B.aSig(SIG_LEN - 1 - i)
    Next

    Dim W(0 To 2 * SIG_LEN + 1) As Ulong
    Dim limbShift As Long = d Shr 5
    Dim bitShift As Long = d And 31
    Dim carryS As Ulongint = 0, tS As Ulongint

    If bitShift = 0 Then
        For i = 0 To SIG_LEN - 1
            W(i + limbShift) = AL(i)
        Next
    Else
        For i = 0 To SIG_LEN - 1
            tS = (Culngint(AL(i)) Shl bitShift) Or carryS
            W(i + limbShift) = Culng(tS And &HFFFFFFFFULL)
            carryS = tS Shr 32
        Next
        W(SIG_LEN + limbShift) = Culng(carryS)
    End If

    Dim n As Long = SIG_LEN + limbShift
    Dim carry As Ulongint = 0, bv As Ulongint, t As Ulongint
    For i = 0 To n
        If i < SIG_LEN Then bv = BL(i) Else bv = 0
        t = Culngint(W(i)) + bv + carry
        W(i) = Culng(t And &HFFFFFFFFULL)
        carry = t Shr 32
    Next
    W(n + 1) = Culng(carry)

    RoundExactLimbs Result, W(), lExpB, False
End Sub

' ==============================================================
' INTERNAL SIGSUB   requires |A| >= |B| and expA >= expB
'
' Exact aligned subtraction is important because cancellation can
' expose low-order bytes that a guard-only scheme would lose.
' ==============================================================
Sub SigSub(Byref Result As BigFloat, Byref A As BigFloat, Byref B As BigFloat, _
           Byval lExpA As Long, Byval lExpB As Long)
    Const BASE64 As Ulongint = 4294967296ULL
    Dim d As Long = lExpA - lExpB
    If d > SIG_BITS Then
        BigFloat_Assign Result, A
        Result.sign = 0
        Exit Sub
    End If

    Dim AL(0 To SIG_LEN - 1) As Ulong
    Dim BL(0 To SIG_LEN - 1) As Ulong
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        AL(i) = A.aSig(SIG_LEN - 1 - i)
        BL(i) = B.aSig(SIG_LEN - 1 - i)
    Next

    Dim W(0 To 2 * SIG_LEN + 1) As Ulong
    Dim limbShift As Long = d Shr 5
    Dim bitShift As Long = d And 31
    Dim carryS As Ulongint = 0, tS As Ulongint
    If bitShift = 0 Then
        For i = 0 To SIG_LEN - 1
            W(i + limbShift) = AL(i)
        Next
    Else
        For i = 0 To SIG_LEN - 1
            tS = (Culngint(AL(i)) Shl bitShift) Or carryS
            W(i + limbShift) = Culng(tS And &HFFFFFFFFULL)
            carryS = tS Shr 32
        Next
        W(SIG_LEN + limbShift) = Culng(carryS)
    End If

    Dim borrow As Ulongint = 0
    Dim av As Ulongint, bv As Ulongint
    Dim n As Long = SIG_LEN + limbShift
    For i = 0 To n
        av = Culngint(W(i))
        If i < SIG_LEN Then bv = Culngint(BL(i)) Else bv = 0
        bv += borrow
        If av < bv Then
            W(i) = Culng(BASE64 + av - bv)
            borrow = 1
        Else
            W(i) = Culng(av - bv)
            borrow = 0
        End If
    Next

    RoundExactLimbs Result, W(), lExpB, False
End Sub

' ==============================================================
' TOSTRING
' ==============================================================
Function BigFloat_ToString(Byref F As BigFloat) As String
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
    Elseif E < 0 Then
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

Sub BigByte_MulSmall(A() As Long, Byval m As Long)
    Dim i As Long
    Dim carry As Long = 0
    Dim t As Long

    For i = Ubound(A) To Lbound(A) Step -1
        t = A(i) * m + carry
        A(i) = t And 255
        carry = t \ 256
    Next

    Do While carry <> 0
        Dim oldUB As Long
        oldUB = Ubound(A)

        Redim Preserve A(0 To oldUB + 1)

        For i = oldUB + 1 To 1 Step -1
            A(i) = A(i - 1)
        Next

        A(0) = carry And 255
        carry \= 256
    Loop
End Sub

Sub BigByte_AddSmall(A() As Long, Byval v As Long)
    Dim i As Long
    Dim carry As Long = v
    Dim t As Long

    i = Ubound(A)

    Do While i >= Lbound(A) Andalso carry <> 0
        t = A(i) + carry
        A(i) = t And 255
        carry = t \ 256
        i -= 1
    Loop

    Do While carry <> 0
        Dim oldUB As Long
        oldUB = Ubound(A)

        Redim Preserve A(0 To oldUB + 1)

        For i = oldUB + 1 To 1 Step -1
            A(i) = A(i - 1)
        Next

        A(0) = carry And 255
        carry \= 256
    Loop
End Sub

Function BigByte_DivSmall(A() As Long, Byval d As Long) As Long
    Dim i As Long
    Dim carry As Long = 0
    Dim t As Long

    For i = Lbound(A) To Ubound(A)
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
    Redim A(0)
    A(0) = 0
End Sub

Sub BigDec_TrimMSZ(A() As mylong)
    Dim i As Long

    For i = Ubound(A) To 1 Step -1
        If A(i) <> 0 Then Exit For
        Redim Preserve A(i - 1)
    Next
End Sub

Sub BigDec_MulSmall(A() As mylong, Byval m As mylong)
    If m = 0 Then
        Redim A(0)
        A(0) = 0
        Exit Sub
    End If

    Dim carry As Long = 0
    Dim i As Long, prod As Long

    For i = 0 To Ubound(A)
        prod = Clng(A(i)) * m + carry
        A(i) = prod Mod 10
        carry = prod \ 10
    Next

    Do While carry > 0
        Redim Preserve A(Ubound(A) + 1)
        A(Ubound(A)) = carry Mod 10
        carry \= 10
    Loop
End Sub

Sub BigDec_AddSmall(A() As mylong, Byval addVal As mylong)
    Dim carry As Long = addVal
    Dim i As Long = 0
    Dim s As Long

    Do While carry > 0
        If i > Ubound(A) Then
            Redim Preserve A(i)
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

    For i = Ubound(A) To 0 Step -1
        sb &= Trim(Str(A(i)))
    Next

    If sb = "" Then sb = "0"

    BigDec_ToString = sb
End Function

' ==============================================================
' IsExpNeg
' ==============================================================
Function IsExpNeg(Byref F As BigFloat) As Boolean
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
Sub BigFloat_Truncate(Byref Result As BigFloat, Byref X As BigFloat)
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

Function BigFloat_TruncatedIsOdd(Byref X As BigFloat) As Boolean
    If IsNaN_F(X) Orelse IsZero_F(X) Then Return False
    If X.exponent > 0 Then Return False

    Dim bitIndex As Long = -X.exponent
    If bitIndex < 0 Orelse bitIndex >= SIG_BITS Then Return False

    Dim limbIndex As Long = SIG_LEN - 1 - (bitIndex Shr 5)
    Dim bitInLimb As Long = bitIndex And 31
    Return (((X.aSig(limbIndex) Shr bitInLimb) And 1UL) <> 0)
End Function

' ==============================================================
' NEGATE
' ==============================================================
Sub BigFloat_Negate(Byref Result As BigFloat, Byref X As BigFloat)

    BigFloat_Assign Result, X

    ' Keep canonical zero positive.
    If Not IsZero_F(Result) Then
        Result.sign Xor= 1
    End If

End Sub


' ==============================================================
' ABSOLUTE VALUE
' ==============================================================
Sub BigFloat_Abs(Byref Result As BigFloat, Byref X As BigFloat)

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
Sub BigFloat_Mul2(Byref Result As BigFloat, Byref X As BigFloat)
    BigFloat_Assign Result, X
    If IsNaN_F(Result) Orelse IsZero_F(Result) Then Exit Sub
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
Sub BigFloat_Div2(Byref Result As BigFloat, Byref X As BigFloat)
    BigFloat_Assign Result, X
    If IsNaN_F(Result) Orelse IsZero_F(Result) Then Exit Sub
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
Sub BigFloat_AddI64(Byref Result As BigFloat, Byref X As BigFloat, Byval v As Longint)
    Dim T As BigFloat
    BigFloat_FromLongInt T, v
    BigFloat_Add Result, X, T
End Sub

Sub BigFloat_AddU64(Byref Result As BigFloat, Byref X As BigFloat, Byval v As Ulongint)
    Dim T As BigFloat
    BigFloat_FromULongInt T, v
    BigFloat_Add Result, X, T
End Sub

Sub BigFloat_SubtractI64(Byref Result As BigFloat, Byref X As BigFloat, Byval v As Longint)
    Dim T As BigFloat
    BigFloat_FromLongInt T, v
    BigFloat_Subtract Result, X, T
End Sub

Sub BigFloat_SubtractU64(Byref Result As BigFloat, Byref X As BigFloat, Byval v As Ulongint)
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
Sub BigFloat_MultiplyU64(Byref Result As BigFloat, Byref X As BigFloat, Byval mag As Ulongint)
    If IsNaN_F(X) Then
        BigFloat_SetNaN Result
        Exit Sub
    End If

    If IsZero_F(X) Orelse mag = 0 Then
        BigFloat_Clear Result
        Exit Sub
    End If

    Dim blNeg As Boolean
    blNeg = IsNeg_F(X)

    Dim XL(0 To SIG_LEN - 1) As Ulong
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        XL(i) = X.aSig(SIG_LEN - 1 - i)
    Next

    Dim ML(0 To 1) As Ulong
    ML(0) = Culng(mag And &HFFFFFFFFULL)
    ML(1) = Culng(mag Shr 32)

    Dim nm As Long
    If ML(1) <> 0 Then
        nm = 2
    Else
        nm = 1
    End If

    ' Exact product needs SIG_LEN+nm limbs (<=16); one spare limb
    ' kept for consistency with the defensive margin used elsewhere.
    Dim P(0 To SIG_LEN + 2) As Ulong

    LimbMulAccumulate XL(), SIG_LEN, ML(), nm, P()

    RoundExactLimbs Result, P(), GetExp(X), False

    If blNeg Andalso Not IsZero_F(Result) Then
        Result.sign = 1
    End If
End Sub

Sub BigFloat_MultiplyI64(Byref Result As BigFloat, Byref X As BigFloat, Byval v As Longint)
    Dim mag As Ulongint
    Dim neg As Boolean

    If v < 0 Then
        neg = True
        ' Avoid overflow at LONGINT_MIN, same trick as BigFloat_FromLongInt.
        mag = Culngint(-(v + 1LL)) + 1ULL
    Else
        neg = False
        mag = Culngint(v)
    End If

    BigFloat_MultiplyU64 Result, X, mag

    If neg Andalso Not IsZero_F(Result) Then
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
Sub BigFloat_DivideU64(Byref Result As BigFloat, Byref X As BigFloat, Byval mag As Ulongint)
    If IsNaN_F(X) Orelse mag = 0 Then
        BigFloat_SetNaN Result
        Exit Sub
    End If

    If IsZero_F(X) Then
        BigFloat_Clear Result
        Exit Sub
    End If

    Dim blNeg As Boolean
    blNeg = IsNeg_F(X)

    Const BASE64 As Ulongint = 4294967296ULL
    Const MASK64 As Ulongint = &HFFFFFFFFULL

    ' numerator = Mx * 2^64: 16 little-endian limbs, low 2 are zero.
    Dim numerator(0 To SIG_LEN + 1) As Ulong
    Dim i As Long
    For i = 0 To SIG_LEN - 1
        numerator(i + 2) = X.aSig(SIG_LEN - 1 - i)
    Next

    Dim d0 As Ulong, d1 As Ulong
    d0 = Culng(mag And MASK64)
    d1 = Culng(mag Shr 32)

    Dim Q(0 To SIG_LEN + 1) As Ulong
    Dim R As Ulongint

    If d1 = 0 Then
        ' Single-limb divisor: remainder always < d0 <= 2^32-1, so it
        ' fits in a limb and each step is one native 64-bit div/mod.
        Dim r As Ulongint = 0
        Dim cur As Ulongint
        Dim k As Long
        For k = SIG_LEN + 1 To 0 Step -1
            cur = (r Shl 32) Or Culngint(numerator(k))
            Q(k) = Culng(cur \ Culngint(d0))
            r = cur Mod Culngint(d0)
        Next
        R = r
    Else
        ' Two-limb divisor: Knuth Algorithm D, m=2.
        Dim s As Long
        s = LeadingZeroBits32(d1)

        Dim vn0 As Ulong, vn1 As Ulong
        If s = 0 Then
            vn0 = d0 : vn1 = d1
        Else
            vn1 = Culng(((Culngint(d1) Shl s) Or (Culngint(d0) Shr (32 - s))) And MASK64)
            vn0 = Culng((Culngint(d0) Shl s) And MASK64)
        End If

        Dim un(0 To SIG_LEN + 2) As Ulong   ' numerator (16 limbs) + 1 normalization limb
        Dim carryN As Ulongint
        If s = 0 Then
            For i = 0 To SIG_LEN + 1
                un(i) = numerator(i)
            Next
        Else
            carryN = 0
            Dim tN As Ulongint
            For i = 0 To SIG_LEN + 1
                tN = (Culngint(numerator(i)) Shl s) Or carryN
                un(i) = Culng(tN And MASK64)
                carryN = tN Shr 32
            Next
            un(SIG_LEN + 2) = Culng(carryN)
        End If

        Dim nq As Long
        nq = SIG_LEN + 1   ' (SIG_LEN+2) - 2 + 1 quotient digits, j = nq-1 downto 0

        Dim j As Long
        Dim qhat As Ulongint, rhat As Ulongint, num As Ulongint
        Dim lhs As Ulongint, rhs As Ulongint
        Dim p As Ulongint, pLow As Ulongint, borrow As Ulongint
        Dim ui As Ulongint, negative As Boolean, sum As Ulongint, carry2 As Ulongint

        For j = nq - 1 To 0 Step -1
            If un(j + 2) = vn1 Then
                qhat = BASE64 - 1
                rhat = Culngint(un(j + 1)) + Culngint(vn1)
            Else
                num = (Culngint(un(j + 2)) Shl 32) Or Culngint(un(j + 1))
                qhat = num \ Culngint(vn1)
                rhat = num Mod Culngint(vn1)
            End If

            Do
                If rhat >= BASE64 Then Exit Do
                lhs = qhat * Culngint(vn0)
                rhs = (rhat Shl 32) Or Culngint(un(j))
                If lhs <= rhs Then Exit Do
                qhat -= 1
                rhat += Culngint(vn1)
            Loop

            borrow = 0
            p = qhat * Culngint(vn0) + borrow
            pLow = p And MASK64
            borrow = p Shr 32
            ui = Culngint(un(j))
            If ui < pLow Then
                un(j) = Culng(BASE64 + ui - pLow)
                borrow += 1
            Else
                un(j) = Culng(ui - pLow)
            End If

            p = qhat * Culngint(vn1) + borrow
            pLow = p And MASK64
            borrow = p Shr 32
            ui = Culngint(un(j + 1))
            If ui < pLow Then
                un(j + 1) = Culng(BASE64 + ui - pLow)
                borrow += 1
            Else
                un(j + 1) = Culng(ui - pLow)
            End If

            ui = Culngint(un(j + 2))
            If ui < borrow Then
                un(j + 2) = Culng(BASE64 + ui - borrow)
                negative = True
            Else
                un(j + 2) = Culng(ui - borrow)
                negative = False
            End If

            If negative Then
                qhat -= 1
                sum = Culngint(un(j)) + Culngint(vn0)
                un(j) = Culng(sum And MASK64)
                carry2 = sum Shr 32
                sum = Culngint(un(j + 1)) + Culngint(vn1) + carry2
                un(j + 1) = Culng(sum And MASK64)
                carry2 = sum Shr 32
                un(j + 2) = Culng((Culngint(un(j + 2)) + carry2) And MASK64)
            End If

            Q(j) = Culng(qhat)
        Next

        Dim r0 As Ulong, r1 As Ulong
        If s = 0 Then
            r0 = un(0) : r1 = un(1)
        Else
            r0 = Culng(((Culngint(un(0)) Shr s) Or (Culngint(un(1)) Shl (32 - s))) And MASK64)
            r1 = Culng(((Culngint(un(1)) Shr s) Or (Culngint(un(2)) Shl (32 - s))) And MASK64)
        End If
        R = Culngint(r0) Or (Culngint(r1) Shl 32)
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
            Dim twoR As Ulongint
            twoR = R Shl 1
            If twoR > mag Then
                roundUp = True
            Elseif twoR = mag Then
                roundUp = ((Q(0) And 1UL) <> 0)   ' tie -> round to even (unreachable here, kept for safety)
            End If
        End If

        If roundUp Then
            Dim carryQ As Ulongint = 1
            Dim ii As Long, xq As Ulongint
            For ii = 0 To SIG_LEN + 1
                xq = Culngint(Q(ii)) + carryQ
                Q(ii) = Culng(xq And MASK64)
                carryQ = xq Shr 32
                If carryQ = 0 Then Exit For
            Next
        End If

        ' RoundExactLimbs just repositions/pads the now-exact value --
        ' no further rounding decision left to make, hence sticky=False.
        RoundExactLimbs Result, Q(), lNewExp, False
    End If

    If blNeg Andalso Not IsZero_F(Result) Then
        Result.sign = 1
    End If
End Sub

Sub BigFloat_DivideI64(Byref Result As BigFloat, Byref X As BigFloat, Byval v As Longint)
    Dim mag As Ulongint

    If v < 0 Then
        mag = Culngint(-(v + 1LL)) + 1ULL   ' avoid overflow at LONGINT_MIN
    Else
        mag = Culngint(v)
    End If

    BigFloat_DivideU64 Result, X, mag

    If v < 0 Andalso Not IsZero_F(Result) Then
        Result.sign Xor= 1
    End If
End Sub

Function BigFloat_HighBit32(Byval x As Ulong) As Long

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
Sub BigFloat_Frexp(Byref X As BigFloat, _
               Byref Mantissa As BigFloat, _
               Byref BinExponent As Long)
    BinExponent = 0
    If IsNaN_F(X) Then BigFloat_SetNaN Mantissa : Exit Sub
    If IsZero_F(X) Then BigFloat_Clear Mantissa : Exit Sub

    BigFloat_Assign Mantissa, X
    BinExponent = X.exponent + SIG_BITS
    Mantissa.exponent = -SIG_BITS
End Sub

Sub BigFloat_FromULongInt(Byref R As BigFloat, Byval v As Ulongint)
    If v = 0 Then BigFloat_Clear R : Exit Sub
    Dim A(0 To 1) As Ulong
    A(0) = Culng(v And &HFFFFFFFFULL)
    A(1) = Culng(v Shr 32)
    RoundExactLimbs R, A(), 0, False
End Sub

Sub BigFloat_FromLongInt(Byref R As BigFloat, Byval v As Longint)

    If v >= 0 Then
        BigFloat_FromULongInt R, Culngint(v)
        Exit Sub
    End If

    Dim mag As Ulongint
    mag = Culngint(-(v + 1LL)) + 1ULL

    BigFloat_FromULongInt R, mag

    If Not IsZero_F(R) Then
        R.sign = 1
    End If

End Sub


Sub BigFloat_FromULong(Byref R As BigFloat, Byval v As Ulong)
    BigFloat_FromULongInt R, Culngint(v)
End Sub


Sub BigFloat_FromLong(Byref R As BigFloat, Byval v As Long)

    If v >= 0 Then
        BigFloat_FromULongInt R, Culngint(v)
    Else
        Dim t As Longint
        t = Clngint(v)

        Dim mag As Ulongint
        mag = Culngint(-(t + 1LL)) + 1ULL

        BigFloat_FromULongInt R, mag
        R.sign = 1
    End If

End Sub

Function BigFloat_ToULongInt(Byref X As BigFloat, Byref v As Ulongint) As Boolean
    v = 0
    If IsNaN_F(X) Then Return False
    If IsZero_F(X) Then Return True
    If IsNeg_F(X) Then Return False

    Dim highValueBit As Longint = Clngint(SIG_BITS - 1) + Clngint(X.exponent)
    If highValueBit < 0 Then Return True
    If highValueBit > 63 Then Return False

    Dim k As Long
    For k = Clng(highValueBit) To 0 Step -1
        v = (v Shl 1) Or Culngint(BigFloat_GetSigBit(X, k - X.exponent))
    Next
    Return True
End Function

Function BigFloat_ToLongInt(Byref X As BigFloat, Byref v As Longint) As Boolean
    v = 0
    If IsNaN_F(X) Then Return False
    If IsZero_F(X) Then Return True

    Dim T As BigFloat
    BigFloat_Assign T, X
    T.sign = 0
    Dim mag As Ulongint
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
        v = Clngint(mag)
    End If
    Return True
End Function

Function BigFloat_ToULong(Byref X As BigFloat, _
                      Byref v As Ulong) As Boolean

    v = 0

    Dim t As Ulongint

    If Not BigFloat_ToULongInt(X, t) Then Return False
    If t > &HFFFFFFFFULL Then Return False

    v = Culng(t)

    Return True

End Function


Function BigFloat_ToLong(Byref X As BigFloat, _
                     Byref v As Long) As Boolean

    v = 0

    Dim t As Longint

    If Not BigFloat_ToLongInt(X, t) Then Return False

    If t < -2147483648LL Orelse _
       t >  2147483647LL Then
        Return False
    End If

    v = Clng(t)

    Return True

End Function

Sub BigFloat_FromDouble(Byref R As BigFloat, Byval v As Double)
    BigFloat_Clear R
    Dim bits As Ulongint
    fb_memcopy(bits, v, 8)

    Dim neg As Boolean = ((bits Shr 63) <> 0)
    Dim expField As Long = Clng((bits Shr 52) And &H7FFULL)
    Dim fraction As Ulongint = bits And &HFFFFFFFFFFFFFULL
    If expField = &H7FF Then BigFloat_SetNaN R : Exit Sub
    If expField = 0 Andalso fraction = 0 Then Exit Sub

    Dim M As Ulongint, e2 As Long
    If expField = 0 Then
        M = fraction
        e2 = -1074
    Else
        M = &H10000000000000ULL Or fraction
        e2 = expField - 1023 - 52
    End If

    Dim A(0 To 1) As Ulong
    A(0) = Culng(M And &HFFFFFFFFULL)
    A(1) = Culng(M Shr 32)
    RoundExactLimbs R, A(), e2, False
    If neg Andalso Not IsZero_F(R) Then R.sign = 1
End Sub

' ==============================================================
' IEEE-754 DOUBLE SUPPORT
' ==============================================================

Union BigFloat_DoubleBits
    d As Double
    u As Ulongint
End Union

Function BigFloat_SigHighBit(Byref X As BigFloat) As Long

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

Function BigFloat_GetSigBit(Byref X As BigFloat, _
                        Byval bitIndex As Long) As Ulong

    If bitIndex < 0 Orelse bitIndex >= SIG_LEN * 32 Then
        Return 0
    End If

    Dim limbIndex As Long
    Dim bitInLimb As Long

    limbIndex = SIG_LEN - 1 - (bitIndex Shr 5)
    bitInLimb = bitIndex And 31

    Return (X.aSig(limbIndex) Shr bitInLimb) And 1UL

End Function

Function BigFloat_AnySigBitsBelow(Byref X As BigFloat, _
                              Byval highBit As Long) As Boolean

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

        Dim mask As Ulong

        mask = (1UL Shl partialBits) - 1UL

        If (X.aSig(limbIndex) And mask) <> 0 Then
            Return True
        End If

    End If

    Return False

End Function

Function BigFloat_RoundShiftToU64(Byref X As BigFloat, _
                              Byval rightShift As Longint) As Ulongint

    Dim highBit As Long
    highBit = BigFloat_SigHighBit(X)

    If highBit < 0 Then Return 0


    Dim q As Ulongint = 0
    Dim i As Long


    If rightShift <= 0 Then

        Dim leftShift As Longint
        leftShift = -rightShift

        For i = highBit To 0 Step -1
            q = (q Shl 1) Or Culngint(BigFloat_GetSigBit(X, i))
        Next

        If leftShift <> 0 Then
            q Shl= Clng(leftShift)
        End If

        Return q

    End If


    If rightShift <= highBit Then

        For i = highBit To Clng(rightShift) Step -1
            q = (q Shl 1) Or Culngint(BigFloat_GetSigBit(X, i))
        Next

    End If


    Dim guard As Boolean = False
    Dim sticky As Boolean = False

    Dim guardIndex As Longint
    guardIndex = rightShift - 1


    If guardIndex >= 0 Andalso guardIndex <= SIG_LEN * 32 - 1 Then
        guard = (BigFloat_GetSigBit(X, Clng(guardIndex)) <> 0)
    End If


    Dim stickyHigh As Longint
    stickyHigh = rightShift - 2

    If stickyHigh >= 0 Then

        If stickyHigh >= SIG_LEN * 32 - 1 Then

            sticky = Not IsZero_F(X)

        Else

            sticky = BigFloat_AnySigBitsBelow(X, Clng(stickyHigh))

        End If

    End If


    If guard Then

        If sticky Orelse ((q And 1ULL) <> 0) Then
            q += 1ULL
        End If

    End If

    Return q

End Function

Function BigFloat_DoubleFromBits(Byval bits As Ulongint) As Double

    Dim U As BigFloat_DoubleBits

    U.u = bits

    Return U.d

End Function

Function BigFloat_ToDouble(Byref X As BigFloat) As Double

    Const DOUBLE_SIGN_MASK As Ulongint = _
        &H8000000000000000ULL

    Const DOUBLE_EXP_MASK As Ulongint = _
        &H7FF0000000000000ULL

    Const DOUBLE_FRAC_MASK As Ulongint = _
        &H000FFFFFFFFFFFFFULL

    Const DOUBLE_QNAN As Ulongint = _
        &H7FF8000000000000ULL

    Const TWO52 As Ulongint = _
        &H0010000000000000ULL

    Const TWO53 As Ulongint = _
        &H0020000000000000ULL


    Dim signBits As Ulongint = 0

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

    Dim binExp As Longint

    binExp = Clngint(h) + Clngint(E)


    If binExp >= -1022LL Then

        Dim shift As Longint

        shift = Clngint(h) - 52LL


        Dim sig53 As Ulongint

        sig53 = BigFloat_RoundShiftToU64(X, shift)


        If sig53 = TWO53 Then

            sig53 Shr= 1
            binExp += 1

        End If


        If binExp > 1023LL Then

            Return BigFloat_DoubleFromBits( _
                signBits Or DOUBLE_EXP_MASK)

        End If


        Dim expField As Ulongint

        expField = _
            Culngint(binExp + 1023LL)


        Dim fracField As Ulongint

        fracField = _
            sig53 And DOUBLE_FRAC_MASK


        Dim bits As Ulongint

        bits = _
              signBits _
            Or (expField Shl 52) _
            Or fracField


        Return BigFloat_DoubleFromBits(bits)

    End If


    Dim subShift As Longint

    subShift = -CLngInt(E) - 1074LL


    Dim subSig As Ulongint

    subSig = BigFloat_RoundShiftToU64(X, subShift)


    If subSig >= TWO52 Then

        Dim bits As Ulongint

        bits = _
              signBits _
            Or &H0010000000000000ULL

        Return BigFloat_DoubleFromBits(bits)

    End If


    If subSig = 0 Then

        Return BigFloat_DoubleFromBits(signBits)

    End If


    Dim bits As Ulongint

    bits = _
          signBits _
        Or subSig

    Return BigFloat_DoubleFromBits(bits)

End Function

' ==============================================================
' BigFloat_Pow2
' ==============================================================
Sub BigFloat_Pow2(Byref Result As BigFloat, Byval n As Long)
    BigFloat_Clear Result
    Result.aSig(0) = &H80000000UL
    Result.exponent = n - (SIG_BITS - 1)
End Sub

Sub BigFloat_Sqrt(Byref result As BigFloat, Byref z As BigFloat)
    Dim As Long ex, e, prec, max=Clng((SIG_LEN * 9.632959861247398))
    Dim As BigFloat x
    Dim As BigFloat m, tmp, r, r2
    Dim As Double dx

    If IsNaN_F(z) Then
        BigFloat_SetNaN result
        Exit Sub
    End If

    If IsZero_F(z) Then
        BigFloat_Clear result
        Exit Sub
    End If

    If IsNeg_F(z) Then
        BigFloat_SetNaN result
        Exit Sub
    End If

    BigFloat_Frexp(z, m, ex)
    If ex And 1 Then
        BigFloat_Add m, m, m
        ex=ex-1
    End If
    e=ex Shr 1 'ex\2
    dx = BigFloat_ToDouble(m)
    dx = Sqr(dx)

    BigFloat_FromDouble r, dx
    prec=32
    Do
        BigFloat_Divide tmp, m, r
        BigFloat_Add r2, r, tmp
        BigFloat_Div2 r, r2
        If prec>max Then Exit Do
        prec*=2
    Loop

    BigFloat_Pow2 r2, e
    BigFloat_Multiply result, r, r2
End Sub

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

Constructor BigFloat_t ( Byval rhs As Ulong )
    BigFloat_FromULong this.BigNum, rhs
End Constructor

Constructor BigFloat_t ( Byval rhs As Ulongint )
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

Operator BigFloat_t.let ( Byval rhs As Longint )
    BigFloat_FromLongInt this.BigNum, rhs
End Operator

Operator BigFloat_t.let ( Byval rhs As Ulong )
    BigFloat_FromULong this.BigNum, rhs
End Operator

Operator BigFloat_t.let ( Byval rhs As Ulongint )
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
    Dim As Long x
    BigFloat_ToLong this.BigNum, x
    Operator = x
End Operator

Operator BigFloat_t.cast ( ) As Longint
    Dim As Longint x
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

Operator + ( Byval lhs As Longint, Byref rhs As BigFloat_t  ) As BigFloat_t
    Dim As BigFloat_t result
    BigFloat_FromLongInt result.BigNum, lhs
    BigFloat_Add result.BigNum, result.BigNum, rhs.BigNum
    Operator = result
End Operator

Operator + (Byref lhs As BigFloat_t, Byref rhs As String) As BigFloat_t
    Dim As BigFloat_t result
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

Operator - ( Byval lhs As Longint, Byref rhs As BigFloat_t  ) As BigFloat_t
    Dim As BigFloat_t result
    BigFloat_FromLongInt result.BigNum, lhs
    BigFloat_Subtract result.BigNum, result.BigNum, rhs.BigNum
    Operator = result
End Operator

Operator - (Byref lhs As BigFloat_t, Byref rhs As String) As BigFloat_t
    Dim As BigFloat_t result
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

Operator * ( Byval lhs As Longint, Byref rhs As BigFloat_t  ) As BigFloat_t
    Dim As BigFloat_t result
    BigFloat_FromLongInt result.BigNum, lhs
    BigFloat_Multiply result.BigNum, result.BigNum, rhs.BigNum
    Operator = result
End Operator

Operator * (Byref lhs As BigFloat_t, Byref rhs As String) As BigFloat_t
    Dim As BigFloat_t result
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

Operator / ( Byval lhs As Longint, Byref rhs As BigFloat_t  ) As BigFloat_t
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

Operator / ( Byval lhs As Long, Byref rhs As BigFloat_t  ) As BigFloat_t
    Dim As BigFloat_t result
    BigFloat_FromLong result.BigNum, lhs
    BigFloat_Divide result.BigNum, result.BigNum, rhs.BigNum
    Operator = result
End Operator

Operator / (Byref lhs As BigFloat_t, Byref rhs As String) As BigFloat_t
    Dim As BigFloat_t result
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

Function fpipow(Byref x As BigFloat_t, Byval e As Longint) As BigFloat_t
    Dim As BigFloat_t one, y, r
    Dim As Longint n

    'take x to an long power

    y = x
    If (e < 0) Then
        n = -e
    Else
        n = e
    End If

    BigFloat_FromLong r.BigNum, 1

    one = r
    While (n > 0)
        While ((n And 1) = 0)
            n=n Shr 1
            y=y*y
        Wend
        n -= 1
        r=y*r
    Wend
    If (e < 0) Then r=one/r
    Return r
End Function

Sub binarysplittingRamanujanPI0(Byval a As Const Ulongint, Byval b As Const Ulongint, Byref p As BigFloat, Byref q As BigFloat, Byref r As BigFloat)
    Dim As BigFloat pp, qq, rr, tmp
    Dim As Ulongint md

    If (b - a) = 1 Then
        If b <= 832256 Then
                        BigFloat_FromULongInt r, ((2 * b) - 1)
                        BigFloat_FromULongInt tmp, ((4 * b) - 3)
                        BigFloat_Multiply r, r, tmp
                        BigFloat_FromULongInt tmp, ((4 * b) - 1)
                        BigFloat_Multiply r, r, tmp
        Else
            BigFloat_FromULongInt r, (((4 * b) - 3) * ((4 * b) - 1))
            BigFloat_FromULongInt tmp, ((2 * b) - 1)
            BigFloat_Multiply r, r, tmp
        End If
        BigFloat_FromULongInt p, (1103ull + (26390ull * b))
        BigFloat_Multiply p, p, r
        BigFloat_FromULongInt q, b
        BigFloat_Multiply tmp, q, q
        BigFloat_Multiply q, q, tmp
        BigFloat_FromULongInt tmp, 3073907232ull
        BigFloat_Multiply q, q, tmp
        Return
    End If
    md = (a + b) \ 2
    binarysplittingRamanujanPI0 a, md, p, q, r
    binarysplittingRamanujanPI0 md, b, pp, qq, rr
    BigFloat_Multiply tmp, p, qq
    BigFloat_Multiply p, pp, r
    BigFloat_Add p, p, tmp
    BigFloat_Multiply q, q, qq
    BigFloat_Multiply r, r, rr
End Sub

Sub Pi_Ramanujan0(Byref result As BigFloat, Byval digits As Ulongint=(SIG_LEN * 9.632959861247398))
    Dim As Ulongint k
    Dim As BigFloat p, q, r
    Dim As BigFloat pi1, tmp
    k = Cast(Ulongint,-int(-( digits*Log(10) / Log(96059301ull) )))
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
End Sub

Sub binarysplittingRamanujanPI(Byval a As Const Ulongint, Byval b As Const Ulongint, Byref p As BigFloat, Byref q As BigFloat, Byref r As BigFloat)
    Dim As BigFloat pp, qq, rr, tmp
    Dim As Ulongint md

    If (b - a) = 1 Then
        If b <= 832256 Then
                        BigFloat_FromULongInt r, ((2 * b) - 1)
                        BigFloat_MultiplyU64 r, r, ((4 * b) - 3)
                        BigFloat_MultiplyU64 r, r, ((4 * b) - 1)
        Else
            BigFloat_FromULongInt r, (((4 * b) - 3) * ((4 * b) - 1))
            BigFloat_MultiplyU64 r, r, ((2 * b) - 1)
        End If
        BigFloat_FromULongInt p, (1103ull + (26390ull * b))
        BigFloat_Multiply p, p, r
        BigFloat_FromULongInt q, b
        BigFloat_Multiply tmp, q, q
        BigFloat_Multiply q, q, tmp
        BigFloat_MultiplyU64 q, q, 3073907232ull
        Return
    End If
    md = (a + b) \ 2
    binarysplittingRamanujanPI a, md, p, q, r
    binarysplittingRamanujanPI md, b, pp, qq, rr
    BigFloat_Multiply tmp, p, qq
    BigFloat_Multiply p, pp, r
    BigFloat_Add p, p, tmp
    BigFloat_Multiply q, q, qq
    BigFloat_Multiply r, r, rr
End Sub

Sub Pi_Ramanujan(Byref result As BigFloat, Byval digits As Ulongint=(SIG_LEN * 9.632959861247398))
    Dim As Ulongint k
    Dim As BigFloat p, q, r
    Dim As BigFloat pi1, tmp
    k = Cast(Ulongint,-int(-( digits*Log(10) / Log(96059301ull) )))
    binarysplittingRamanujanPI(0, k, p, q, r)
    BigFloat_MultiplyU64 pi1, q, 9801ull
    BigFloat_MultiplyU64 tmp, q, 1103ull
    BigFloat_Add tmp, p, tmp
    BigFloat_Divide pi1, pi1, tmp
    BigFloat_FromULongInt tmp, 8ull
    BigFloat_Sqrt tmp, tmp
    BigFloat_Divide result, pi1, tmp
End Sub

Function agm(Byref a_in As BigFloat_t, Byref b_in As BigFloat_t) As BigFloat_t
    Dim As BigFloat_t a, b, c, d, eps

    a=a_in
    b=b_in
    eps="1e-"+Trim(Str((SIG_LEN * 9.632959861247398)-4))
    Do
        c = (a + b)/2
        d = Sqr(a*b)
        a = c
        b = d
    Loop Until Abs(a-b)<eps
    Return a
End Function

Sub calcln2()
    Dim  As BigFloat_t one
    BigFloat_FromLong one.BigNum, 1
    BigFloat_FromLong fpln2.BigNum, 2
    fpln2 = fpipow(fpln2, (-2*Clng((SIG_LEN * 9.632959861247398)+4) + 1))
    fpln2 = agm(one, fpln2)
    fpln2 = (fppi/(2*fpln2))/(2*Clng((SIG_LEN * 9.632959861247398)+4) + 1)
End Sub


Sub init_constants
    Pi_Ramanujan fppi.BigNum
    calcln2
    BigFloat_Mul2 fppi2.BigNum, fppi.BigNum
    BigFloat_Div2 fpPihalf.BigNum, fppi.BigNum
    BigFloat_MultiplyU64 Pi3o2.BigNum, fppi.BigNum, 3
    BigFloat_Div2 Pi3o2.BigNum, Pi3o2.BigNum
    SIN_TAYLOR_N = -int(-(11.09035488895913*SIG_LEN/(Log(1143.335555562796*SIG_LEN/2.718281828459045)-log(Log(1143.335555562796*SIG_LEN/2.718281828459045)))-3/2))
End Sub

Function log_agm(Byref x As BigFloat_t) As BigFloat_t
    Dim As BigFloat_t y, one=1
    Dim As Long digits=Clng((SIG_LEN * 9.632959861247398))
    
    If x <= 0 Then
        Print "can nott take fplog_agm of zero or a negative number"
        Return 0
    End If

    If x=1 Then Return 0

    If x < 1 Then
        y=2
        y=fpipow(y, 2 - 2 * digits)
        y=y*x
        y=fppi/(2*agm(one, y))-fpln2 * 2 * digits
        y=-y
    Else
    
        y=2
        y=fpipow(y, 2 - 2 * digits)
        y=y/x
        y=fppi/(2*agm(one, y))-fpln2 * 2 * digits
    End If
    Return y
End Function

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

Function fpSin(Byref z As BigFloat_t) As BigFloat_t
    Dim As BigFloat_t sn, x2, x = z
    Dim As Long sign

    If Not TaylorSinC_Ready Then InitTaylorSinCoeffs()

    sign = 1
    If x < 0 Then
        sign = -1
        x = -x
    End If

    If x > fpPi2 Then
        sn = x / fpPi2
        BigFloat_Truncate x2.BigNum, sn.BigNum
        x = sn - x2
        x = x * fpPi2
    End If

    If x > fpPihalf Then
        If x < Pi3o2 Then
            x = fpPi - x
        Else
            x = x - fpPi2
        End If
    End If

    x = x / 81
    x2 = x * x

    sn = TaylorSinC(SIN_TAYLOR_N)
    For i As Long = SIN_TAYLOR_N - 1 To 0 Step -1
        sn = sn * x2 + TaylorSinC(i)
    Next i
    sn = x * sn

    If sign < 0 Then sn = -sn

    x2 = 3
    sn = sn * (x2 - 4*sn*sn)
    sn = sn * (x2 - 4*sn*sn)
    sn = sn * (x2 - 4*sn*sn)
    sn = sn * (x2 - 4*sn*sn)

    Return sn
End Function

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

Function fpCos(Byref z As BigFloat_t) As BigFloat_t
    Dim As BigFloat_t cs, x2, x = z
    Dim As Long cossign

    If Not TaylorCosC_Ready Then InitTaylorCosCoeffs()

    x = Abs(x)   ' cos is even -- no input-sign tracking needed at all

    If x > fpPi2 Then
        Dim As BigFloat_t sn, xi
        sn = x / fpPi2
        BigFloat_Truncate xi.BigNum, sn.BigNum
        x = sn - xi
        x = x * fpPi2
    End If

    cossign = 1
    If x > fpPihalf Then
        If x < Pi3o2 Then
            x = fpPi - x
            cossign = -1        ' cos(pi-x) = -cos(x) -- sine's reflection identity doesn't carry over
        Else
            x = x - fpPi2       ' cos(x-2pi) = cos(x) -- same as sine here, no flip
        End If
    End If

    x = x / 81
    x2 = x * x

    cs = TaylorCosC(COS_TAYLOR_N)

    For i As Long = COS_TAYLOR_N - 1 To 0 Step -1
    
        cs = cs * x2 + TaylorCosC(i)

    Next i
    ' no "cs = x*cs" here -- cosine's series has no leading x factor

    If cossign < 0 Then cs = -cs

    Dim As BigFloat_t three = 3
    cs = cs * (4*cs*cs - three)
    cs = cs * (4*cs*cs - three)
    cs = cs * (4*cs*cs - three)
    cs = cs * (4*cs*cs - three)

    Return cs
End Function

Function fpCoss(Byref z As BigFloat_t) As BigFloat_t
    Dim As BigFloat_t sn, x2, x = fpPihalf-z
    Dim As Long sign

    If Not TaylorSinC_Ready Then InitTaylorSinCoeffs()

    sign = 1
    If x < 0 Then
        sign = -1
        x = -x
    End If

    If x > fpPi2 Then
        sn = x / fpPi2
        BigFloat_Truncate x2.BigNum, sn.BigNum
        x = sn - x2
        x = x * fpPi2
    End If

    If x > fpPihalf Then
        If x < Pi3o2 Then
            x = fpPi - x
        Else
            x = x - fpPi2
        End If
    End If

    x = x / 81
    x2 = x * x

    sn = TaylorSinC(SIN_TAYLOR_N)
    For i As Long = SIN_TAYLOR_N - 1 To 0 Step -1
        sn = sn * x2 + TaylorSinC(i)
    Next i
    sn = x * sn

    If sign < 0 Then sn = -sn

    x2 = 3
    sn = sn * (x2 - 4*sn*sn)
    sn = sn * (x2 - 4*sn*sn)
    sn = sn * (x2 - 4*sn*sn)
    sn = sn * (x2 - 4*sn*sn)

    Return sn
End Function

Sub BigFloat_Exp(Byref Result As BigFloat, Byref X As BigFloat)

    Dim As BigFloat xx
    Dim As BigFloat q
    Dim As BigFloat qround
    Dim As BigFloat half
    Dim As BigFloat kfp
    Dim As BigFloat r

    Dim As BigFloat term
    Dim As BigFloat sum
    Dim As BigFloat oldsum
    Dim As BigFloat tmp

    Dim As Longint k
    Dim As Longint newExp
    Dim As Ulongint n
    Dim As Long i, maxn = Clng((SIG_LEN * 2.4082399653118495))

    ' ----------------------------------------------------------
    ' Special cases
    ' ----------------------------------------------------------
    If IsNaN_F(X) Then
        BigFloat_SetNaN Result
        Exit Sub
    End If

    If IsZero_F(X) Then
        BigFloat_FromLong Result, 1
        Exit Sub
    End If

    ' Alias safety.
    BigFloat_Assign xx, X


    ' ----------------------------------------------------------
    ' q = x / ln(2)
    ' ----------------------------------------------------------
    BigFloat_Divide q, xx, fpln2.BigNum


    ' ----------------------------------------------------------
    ' k = nearest integer(x / ln(2))
    ' ----------------------------------------------------------
    BigFloat_Pow2 half, -1      ' 0.5 exactly

    If IsNeg_F(q) Then
        BigFloat_Subtract qround, q, half
    Else
        BigFloat_Add qround, q, half
    End If

    BigFloat_Truncate qround, qround

    If BigFloat_ToLongInt(qround, k) = False Then

        If IsNeg_F(xx) Then
            BigFloat_Clear Result
        Else
            BigFloat_SetNaN Result
        End If

        Exit Sub
    End If


    ' ----------------------------------------------------------
    ' r = x - k*ln(2)
    '
    ' Therefore:
    '
    '     |r| <= ln(2)/2
    ' ----------------------------------------------------------
    BigFloat_FromLongInt kfp, k

    BigFloat_Multiply tmp, fpln2.BigNum, kfp
    BigFloat_Subtract r, xx, tmp


    ' ----------------------------------------------------------
    ' Additional range reduction:
    '
    '     exp(r) = exp(r/16)^16
    '
    ' Since 16 = 2^4:
    '
    '     exp(r) =
    '         ((((exp(r/16))^2)^2)^2)^2)
    '
    ' Now:
    '
    '     |r/16| <= ln(2)/32
    '            ~= 0.02166084939
    '
    ' which makes the Taylor series converge very rapidly.
    ' ----------------------------------------------------------

    r.exponent -= 4


    ' ----------------------------------------------------------
    ' Taylor series for exp(r/16)
    '
    '     sum  = 1
    '     term = 1
    '
    '     term(n) = term(n-1) * r / n
    '
    ' Stop when the next term no longer changes the rounded
    ' BigFloat result.
    ' ----------------------------------------------------------
    BigFloat_FromLong sum, 1
    BigFloat_FromLong term, 1

    n = 1

    Do

        BigFloat_Multiply tmp, term, r
        BigFloat_DivideU64 term, tmp, n

        BigFloat_Assign oldsum, sum
        BigFloat_Add sum, sum, term

        If BigFloat_Compare(sum, oldsum) = 0 Then Exit Do

        n += 1

        ' Defensive limit only.
        If n > maxn Then Exit Do

    Loop

    ' ----------------------------------------------------------
    ' Undo reduction:
    '
    '     exp(r) = exp(r/16)^16
    '
    ' Four squarings.
    ' ----------------------------------------------------------
    For i = 1 To 4
        BigFloat_Multiply tmp, sum, sum
        BigFloat_Assign sum, tmp
    Next


    ' ----------------------------------------------------------
    ' exp(x) = exp(r) * 2^k
    '
    ' Multiplication by 2^k is exact in the new representation.
    ' ----------------------------------------------------------
    newExp = Clngint(sum.exponent) + k

    If newExp > 2147483647LL Then
        BigFloat_SetNaN Result
        Exit Sub
    End If

    If newExp < -2147483648LL Then
        BigFloat_Clear Result
        Exit Sub
    End If

    BigFloat_Assign Result, sum
    Result.exponent = Clng(newExp)

End Sub

Function fpExp(Byref x As BigFloat_t) As BigFloat_t

    Dim As BigFloat_t result

    BigFloat_Exp result.BigNum, x.BigNum

    Return result

End Function

Function fpSqr(Byref x As BigFloat_t) As BigFloat_t

    Dim As BigFloat_t result

    BigFloat_Sqrt result.BigNum, x.BigNum

    Return result

End Function
