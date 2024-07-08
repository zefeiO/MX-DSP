import chisel3._
import chisel3.util._
import Consts._

class FloatWrapper(expW: Int, mantW: Int, expBias: Int, num: Bits) {
    val totalWidth = 1 + expW + mantW
    require(num.getWidth == totalWidth, s"Input width must be $totalWidth bits (${num.getWidth} instead).")

    val sign = num(totalWidth - 1).asBool
    val exp = num(totalWidth - 2, mantW)
    val bias = expBias
    val mant = Cat(
        Mux(exp === 0.U(expW.W), 0.U(1.W), 1.U(1.W)),   // check if denormalized
        num(mantW - 1, 0)
    )

    def +(that: FloatWrapper): FloatWrapper = {
        require(this.expW == that.exp.getWidth && this.mantW == that.mant.getWidth - 1, "Widths must match")

        // Align exponents
        val expDiff = this.exp.asSInt - that.exp.asSInt
        val alignedMantA = Mux(expDiff > 0.S, this.mant, this.mant >> (-expDiff).asUInt)
        val alignedMantB = Mux(expDiff > 0.S, that.mant >> expDiff.asUInt, that.mant)
        val resultExp = Mux(expDiff > 0.S, this.exp, that.exp)
        
        val extMantA = Cat(0.U(1.W), alignedMantA)
        val extMantB = Cat(0.U(1.W), alignedMantB)
        val mantSum = Mux(this.sign === that.sign, 
            extMantA + extMantB, 
            extMantA - extMantB
        ) // width: mantW + 2

        // handle overflow
        val signFinal = Wire(Bool())
        val expFinal = Wire(UInt(expW.W))
        val mantFinal = Wire(UInt((mantW + 1).W))
        when (mantSum(mantW + 1) === 1.U) {
            when (this.sign === that.sign) {    // addition overflow
                signFinal := this.sign
                mantFinal := mantSum(mantW + 1, 1)
                expFinal := resultExp + 1.U
            }.otherwise {                       // subtraction overflow
                signFinal := !this.sign
                mantFinal := -mantSum(mantW, 0)
                expFinal := resultExp
            }
        }.otherwise {
            signFinal := this.sign
            expFinal := resultExp
            mantFinal := mantSum(mantW, 0)
        }

        // Normalize result
        val normShift = PriorityEncoder(Reverse(mantFinal))

        new FloatWrapper(expW, mantW, this.bias, Mux(mantFinal === 0.U,
            Cat(signFinal, 0.U(expW.W), 0.U(mantW.W)),
            Cat(
                signFinal,
                expFinal - normShift,
                (mantFinal << normShift)(mantW - 1, 0)   // drop leading one
            )
        ))
    }

    def *(that: FloatWrapper): FloatWrapper = {
        val resultSign = this.sign ^ that.sign
        val resultExp = this.exp + that.exp - that.bias.U
        val resultMant = this.mant * that.mant  // width: 2*(mantW+1)
        
        // Round inputs into N-bit outputs:
        // - Rounding up if N-th bit from left is one
        // - Rounding down if N-th bit from left is zero
        // Output width: N + 1 (additional one in case of overflow)
        def round(N: Int)(in: UInt): UInt = {
            require(in.getWidth > N, s"Rounding requires at least $N-bit input")
            in(in.getWidth - 1, in.getWidth - N) + in(in.getWidth - N - 1)
        }

        val roundedMant = Wire(UInt((mantW + 2).W))
        val finalExp = Wire(UInt(expW.W))
        val finalMant = Wire(UInt(mantW.W))
        when (resultMant === 0.U) {
            finalExp := 0.U
            roundedMant := 0.U
            finalMant := 0.U
        }.elsewhen (resultMant(2*(mantW + 1) - 1) === 1.U(1.W)) {
            roundedMant := round(mantW + 1)(resultMant)
            finalExp := resultExp + 1.U
            finalMant := roundedMant(mantW - 1, 0)  // rounding will never overflow in this case
        }.otherwise {
            roundedMant := round(mantW + 1)(resultMant.tail(1))
            when (roundedMant(mantW + 1) === 1.U(1.W)) {
                finalExp := resultExp + 1.U
                finalMant := roundedMant(mantW, 1)  // get first mantW+1 bits and drop leading one
            }.otherwise {
                finalExp := resultExp
                finalMant := roundedMant(mantW - 1, 0)    // drop first zero and drop leading one from the following mantW+1 bits
            }
        }

        new FloatWrapper(expW, mantW, this.bias, Cat(
            resultSign, finalExp, finalMant
        ))
    }

    def scale(factor: UInt): FloatWrapper = {
        require(factor.getWidth == 8, "Shared exponent must be 8 bits long")
        // TODO: remove bias from factor

        new FloatWrapper(factor.getWidth, mantW, this.bias + EXP_BIAS(SHARED_EXP_W), Cat(
            sign, 
            (exp + factor)(factor.getWidth - 1, 0),
            mant(mantW - 1, 0)
        ))
    }
    
    def asFP32(): FloatWrapper = {
        require(expW <= 8, "Exponent width must be less than or equal to 8 bits")
        require(mantW <= 23, "Mantissa width must be less than or equal to 23 bits")

        new FloatWrapper(8, 23, EXP_BIAS(SHARED_EXP_W), Cat(
            sign,
            0.U((8 - expW).W),
            exp,
            mant(mantW - 1, 0),
            0.U((23 - mantW).W)
        ))
    }

    def asBits(): Bits = Cat(sign, exp, mant(mantW - 1, 0))
}
