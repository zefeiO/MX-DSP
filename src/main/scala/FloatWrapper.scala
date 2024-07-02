import chisel3._
import chisel3.util._
import scala.math.pow

class FloatWrapper(expW: Int, mantW: Int, num: Bits) {
    val totalWidth = 1 + expW + mantW
    require(num.getWidth == totalWidth, s"Input width must be $totalWidth bits (${num.getWidth} instead).")

    val sign = num(totalWidth - 1).asBool
    val exp = num(totalWidth - 2, mantW)
    val mant = Cat(
        Mux(exp === 0.U(expW.W), 0.U(1.W), 1.U(1.W)),   // check if denormalized
        num(mantW - 1, 0)
    )

    val expBias = pow(2, expW - 1).toInt - 1

    def +(that: FloatWrapper): FloatWrapper = {
        require(this.expW == that.exp.getWidth && this.mantW == that.mant.getWidth - 1, "Widths must match")

        // Align exponents
        val expDiff = this.exp.asSInt - that.exp.asSInt
        val alignedMantA = Mux(expDiff > 0.S, this.mant, this.mant >> (-expDiff).asUInt)
        val alignedMantB = Mux(expDiff > 0.S, that.mant >> expDiff.asUInt, that.mant)
        val resultExp = Mux(expDiff > 0.S, this.exp, that.exp)

        val resultSign = Mux(
            this.sign === that.sign, 
            this.sign, 
            Mux(alignedMantA > alignedMantB, this.sign, that.sign)
        )
        
        val extMantA = Cat(0.U(1.W), alignedMantA)
        val extMantB = Cat(0.U(1.W), alignedMantB)
        val mantSum = Mux(  // width: mantW + 2
            this.sign === that.sign, 
            extMantA + extMantB, 
            extMantA - extMantB
        )

        // handle overflow
        val signFinal = Wire(Bool())
        val expFinal = Wire(UInt(expW.W))
        val mantFinal = Wire(UInt((mantW + 1).W))
        when (mantSum(mantW + 1) === 1.U) {
            when (this.sign === that.sign) {    // addition overflow
                signFinal := resultSign
                mantFinal := mantSum(mantW + 1, 1)
                expFinal := resultExp + 1.U
            }.otherwise {                       // subtraction overflow
                signFinal := !resultSign
                mantFinal := -mantSum(mantW, 0) // TODO: check if correct
                expFinal := resultExp
            }
        }.otherwise {
            signFinal := resultSign
            mantFinal := mantSum(mantW, 0)
            expFinal := resultExp
        }

        // Normalize result
        val normShift = PriorityEncoder(Reverse(mantFinal))

        new FloatWrapper(expW, mantW, Mux(mantFinal === 0.U,
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
        val resultExp = this.exp + that.exp
        val resultMant = this.mant * that.mant  // width: 2*(mantW+1)
        
        def round(N: Int)(in: UInt): UInt = {
            require(in.getWidth > N, s"Rounding requires at least $N-bit input")
            in(in.getWidth - 1, in.getWidth - N + 1) + in(in.getWidth - N)
        }

        val finalExp = Wire(UInt(expW.W))
        val finalMant = Wire(UInt(mantW.W))
        when (resultMant === 0.U) {
            finalExp := 0.U
            finalMant := 0.U
        }.elsewhen (resultMant(2*(mantW + 1) - 1) === 1.U(1.W)) {
            finalExp := resultExp + 1.U - expBias.U
            finalMant := round(mantW)(resultMant)
        }.otherwise {
            finalExp := resultExp - expBias.U
            finalMant := round(mantW)(resultMant.tail(1))
        }

        new FloatWrapper(expW, mantW, Cat(
            resultSign, finalExp, finalMant
        ))
    }

    def scale(factor: UInt): FloatWrapper = 
        new FloatWrapper(factor.getWidth, mantW, Cat(
            sign, (exp + factor)(factor.getWidth - 1, 0), mant(mantW - 1, 0)))
    
    def asFP32(): FloatWrapper = {
        require(expW <= 8, "Exponent width must be less than or equal to 8 bits")
        require(mantW <= 23, "Mantissa width must be less than or equal to 23 bits")

        new FloatWrapper(8, 23, Cat(
            sign,
            0.U((8 - expW).W),
            exp,
            mant(mantW - 1, 0),
            0.U((23 - mantW).W)
        ))
    }

    def asBits(): Bits = Cat(sign, exp, mant(mantW - 1, 0))
}
