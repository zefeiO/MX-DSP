import chisel3._
import chisel3.util._
import Consts._

class FloatWrapper(expW: Int, mantW: Int, expBias: Int, num: Bits, scaling: Boolean = false) {
    // Scala variables
    val TOTAL_WIDTH = 1 + expW + mantW
    val BIAS = expBias
    val MAX_NORMAL = MAX_NORMAL_MAP((expW, mantW))

    // Data wires
    val sign = Wire(Bool())
    val exp = Wire(UInt(expW.W))
    val mant = Wire(UInt((mantW + 1).W))
    val subnormal = Wire(Bool())

    if (scaling) {
        // in scaling constructor, num contain an extra bit to specify first bit of mantissa explicitly
        require(num.getWidth == TOTAL_WIDTH + 1, s"Scaling Input width must be ${TOTAL_WIDTH+1} bits (${num.getWidth} instead).")
        sign := num(TOTAL_WIDTH)
        exp := num(TOTAL_WIDTH - 1, mantW + 1)
        mant := num(mantW, 0)
        subnormal := mant(mantW) === 0.U
    } else {
        require(num.getWidth == TOTAL_WIDTH, s"Input width must be $TOTAL_WIDTH bits (${num.getWidth} instead).")
        sign := num(TOTAL_WIDTH - 1)
        when (num(TOTAL_WIDTH - 2, mantW) === 0.U) {    // subnormal number
            exp := 1.U
            mant := Cat(0.U(1.W), num(mantW - 1, 0))
            subnormal := 1.B
        }.otherwise {
            exp := num(TOTAL_WIDTH - 2, mantW)
            mant := Cat(1.U(1.W), num(mantW - 1, 0))
            subnormal := 0.B
        }
    }

    def +(that: FloatWrapper): FloatWrapper = {
        require(this.expW == that.exp.getWidth && this.mantW == that.mant.getWidth - 1, "Widths must match")

        /* Align exponents */
        val expDiff = this.exp.asSInt - that.exp.asSInt
        val alignedMantA = Mux(expDiff > 0.S, this.mant, this.mant >> (-expDiff).asUInt)
        val alignedMantB = Mux(expDiff > 0.S, that.mant >> expDiff.asUInt, that.mant)
        val exp1 = Mux(expDiff > 0.S, this.exp, that.exp)
        
        val extMantA = Cat(0.U(1.W), alignedMantA)  // width: mantW + 2
        val extMantB = Cat(0.U(1.W), alignedMantB)  // width: mantW + 2
        val mant1 = Mux(this.sign === that.sign, 
            extMantA + extMantB,
            extMantA - extMantB
        ) // width: mantW + 2

        /* Handle overflow in mantissa addition */
        val signFinal = Wire(Bool())
        val exp2 = Wire(UInt((expW + 1).W))
        val mant2 = Wire(UInt((mantW + 1).W))
        when (mant1(mantW + 1) === 1.U) {
            when (this.sign === that.sign) {    // addition overflow
                signFinal := this.sign
                mant2 := mant1(mantW + 1, 1)
                exp2 := exp1 + 1.U
            }.otherwise {                       // subtraction overflow
                signFinal := !this.sign
                mant2 := -mant1(mantW, 0)
                exp2 := exp1
            }
        }.otherwise {
            signFinal := this.sign
            exp2 := exp1
            mant2 := mant1(mantW, 0)
        }

        /* Normalize result */
        val normShift = PriorityEncoder(Reverse(mant2))
        val exp3 = Wire(UInt((expW + 1).W))
        val mant3 = Wire(UInt((mantW).W))
        when (normShift >= exp2) {  // subnormal number
            // Example 1
            // exp2=0001, mant2=0.111 => normShift=1
            // result: exp3=0000, mant3=0.111
            // Example 2
            // exp2=0001, mant2=0.011 => normShift=2
            // result: exp3=0000, mant3=0.011
            // Example 3
            // exp2=0010, mant2=0.011 => normShift=2
            // result: exp3=0000, mant3=0.110
            exp3 := 0.U
            mant3 := (mant2 << (exp2 - 1.U))(mantW - 1, 0)
        }.otherwise {   // normal number
            exp3 := exp2 - normShift
            mant3 := (mant2 << normShift)(mantW - 1, 0) // drop leading one
        }

        /* Handle saturation */
        val exp4 = Wire(UInt(expW.W))
        val mant4 = Wire(UInt(mantW.W))
        when (Cat(exp3, mant3) > MAX_NORMAL.U) {
            exp4 := MAX_NORMAL.U(expW + mantW - 1, mantW)
            mant4 := MAX_NORMAL.U(mantW - 1, 0)
        }.otherwise {
            exp4 := exp3(expW - 1, 0)
            mant4 := mant3
        }

        new FloatWrapper(expW, mantW, this.BIAS, Mux(mant2 === 0.U,
            Cat(signFinal, 0.U(expW.W), 0.U(mantW.W)),
            Cat(signFinal, exp4, mant4)
        ))
    }

    def *(that: FloatWrapper): FloatWrapper = {
        require(this.expW == that.exp.getWidth && this.mantW == that.mant.getWidth - 1, "Widths must match")

        val extExpA = Cat(0.U(2.W), this.exp)
        val extExpB = Cat(0.U(2.W), that.exp)

        val resultSign = this.sign ^ that.sign
        val exp1 = Wire(SInt((expW + 2).W))
        exp1 := extExpA.asSInt + extExpB.asSInt - that.BIAS.S    // width: expW+2
        val mant1 = this.mant * that.mant  // width: 2*(mantW+1)

        /* Normalize result */
        val normShift = PriorityEncoder(Reverse(mant1))
        val exp2 = exp1 - normShift.asSInt + 1.S    // width: expW+2
        val mant2 = (mant1 << normShift)(2*mantW + 1, mantW + 1) // width: mantW+1

        /* Handle subnormal numbers */
        val exp3 = Wire(UInt((expW + 1).W))
        val mant3 = Wire(UInt(mantW.W))
        when (exp2 <= 0.S) {
            exp3 := 0.U
            mant3 := (mant2 >> (1.S - exp2).asUInt)(mantW - 1, 0)   // drop leading zero
        }.otherwise {
            exp3 := exp2(expW, 0)
            mant3 := mant2(mantW - 1, 0)    // drop leading one
        }

        /* Handle saturation */
        val exp4 = Wire(UInt(expW.W))
        val mant4 = Wire(UInt(mantW.W))
        when (Cat(exp3, mant3) > MAX_NORMAL.U) {
            exp4 := MAX_NORMAL.U(expW + mantW - 1, mantW)
            mant4 := MAX_NORMAL.U(mantW - 1, 0)
        }.otherwise {
            exp4 := exp3(expW - 1, 0)
            mant4 := mant3
        }

        new FloatWrapper(expW, mantW, this.BIAS, Mux(mant4 === 0.U,
            Cat(resultSign, 0.U(expW.W), 0.U(mantW.W)),
            Cat(resultSign, exp4, mant4)
        ))
    }

    def scale(factor: UInt): FloatWrapper = {
        require(factor.getWidth == SHARED_EXP_W, s"Shared exponent must be $SHARED_EXP_W bits long")

        new FloatWrapper(factor.getWidth, mantW, this.BIAS + EXP_BIAS(SHARED_EXP_W), Cat(
            sign, 
            (exp + factor)(factor.getWidth - 1, 0),
            mant(mantW, 0)
        ), true)
    }
    
    def asFP32(): FloatWrapper = {
        require(expW <= 8, "Exponent width must be less than or equal to 8 bits")
        require(mantW <= 23, "Mantissa width must be less than or equal to 23 bits")

        new FloatWrapper(8, 23, EXP_BIAS(8), Cat(
            sign,
            0.U((8 - expW).W),
            exp - BIAS.U + EXP_BIAS(8).U,
            mant(mantW - 1, 0),
            0.U((23 - mantW).W)
        ))
    }

    def asBits(): Bits = Mux(subnormal,
            Cat(sign, exp - 1.U, mant(mantW - 1, 0)),
            Cat(sign, exp, mant(mantW - 1, 0))
        )

    // Round inputs into N-bit outputs:
    // - Rounding up if N-th bit from left is one
    // - Rounding down if N-th bit from left is zero
    // Output width: N + 1 (additional one in case of overflow)
    def round(N: Int)(in: UInt): UInt = {
        require(in.getWidth > N, s"Rounding requires at least $N-bit input")
        in(in.getWidth - 1, in.getWidth - N) + in(in.getWidth - N - 1)
    }
}
