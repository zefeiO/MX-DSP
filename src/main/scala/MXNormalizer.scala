import chisel3._
import chisel3.util._

class MXNormalizer(k: Int, inExpW: Int, outExpW: Int, mantW: Int) extends Module {
    val inElementW = 1 + inExpW + mantW
    val outElementW = 1 + outExpW + mantW

    val io = IO(new Bundle {
        val inElements = Input(Vec(k, UInt(inElementW.W)))
        val outElements = Output(Vec(k, UInt(outElementW.W)))
        val outSharedExp = Output(UInt(8.W))
    })

    val maxElementExp = io.inElements.map(_(inElementW - 2, mantW)).reduce((a, b) => Mux(a > b, a, b))
    val outSharedExp = maxElementExp - Fill(outExpW, 1.U(1.W))

    io.outElements := io.inElements.map{ case inElement => Cat(
        inElement(inElementW),
        (inElement(inElementW - 2, mantW) - outSharedExp)(outExpW - 1, 0),
        inElement(mantW - 1, 0)
    )}
    io.outSharedExp := outSharedExp
}