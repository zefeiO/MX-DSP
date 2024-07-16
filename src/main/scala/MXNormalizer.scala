import chisel3._
import chisel3.util._
import Consts._

class MXNormalizer(k: Int, inExpW: Int, outExpW: Int, mantW: Int, normalize: Boolean) extends Module {
    val inElementW = 1 + inExpW + mantW
    val outElementW = 1 + outExpW + mantW

    val io = IO(new Bundle {
        val inElements = Input(Vec(k, Bits(inElementW.W)))
        val scale = if (normalize) None else Some(Input(UInt(SHARED_EXP_W.W)))
        val outElements = Output(Vec(k, Bits(outElementW.W)))
        val outSharedExp = Output(UInt(8.W))
    })

    val maxElementExp = io.inElements.map(_(inElementW - 2, mantW)).reduce((a, b) => Mux(a > b, a, b))

    val EMAX_ELEM = Fill(outExpW, 1.U(1.W))
    val outSharedExp = Wire(UInt(SHARED_EXP_W.W))

    io.scale match {
        case Some(scale) => outSharedExp := scale
        case None => when (maxElementExp >= EMAX_ELEM) {
                outSharedExp := maxElementExp - EMAX_ELEM
            }.otherwise {
                outSharedExp := 0.U
            }
    }

    // TODO: handle denormalized numbers when scale is specified?
    io.outElements := io.inElements.map{ case inElement => Cat(
        inElement(inElementW - 1),
        (inElement(inElementW - 2, mantW) - outSharedExp)(outExpW - 1, 0),
        inElement(mantW - 1, 0)
    )}
    io.outSharedExp := outSharedExp
}
