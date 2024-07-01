import chisel3._
import chisel3.util._

class FloatWrapper(num: UInt, expW: Int, mantW: Int) {
    val totalWidth = 1 + expW + mantW
    require(num.getWidth == totalWidth, s"Input width must be $totalWidth bits")

    val sign = num(totalWidth - 1).asBool
    val exp = num(totalWidth - 2, mantW)
    val mant = Cat(
        Mux(exp === 0.U(expW.W), 0.U(1.W), 1.U(1.W)),   // check if denormalized
        num(mantW - 1, 0)
    )

    val data = Cat(sign, exp, mant)

    def +(that: FloatWrapper): FloatWrapper = {
        require(this.expW == that.exp.getWidth && this.mantW == that.mant.getWidth, "Widths must match")

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

        val mantSum = Mux(
            this.sign === that.sign, 
            alignedMantA + alignedMantB, 
            alignedMantA - alignedMantB
        )

        // Normalize result
        val normShift = PriorityEncoder(Reverse(mantSum))

        val resultNum = Wire(UInt(totalWidth.W))

        // if the mantissa sum is zero, result mantissa and exponent should be zero
        when (mantSum === 0.U) {
            resultNum := Cat(resultSign, 0.U(expW.W), 0.U(mantW.W))
        }.otherwise {
            resultNum := Cat(
                resultSign,
                (mantSum << normShift)(mantW - 1, 0),
                resultExp - normShift
            )
        }

        new FloatWrapper(resultNum, expW, mantW)
    }

    def *(that: FloatWrapper): FloatWrapper = {
        this
    }

    def scale(factor: UInt): FloatWrapper = {
        new FloatWrapper(Cat(
                sign,
                (exp + factor)(factor.getWidth - 1, 0),
                mant
            ),
            factor.getWidth,
            mantW
        )  
    }
}
