import chisel3._
import chisel3.experimental.VecLiterals._
import chiseltest._
import chiseltest.experimental.expose
import org.scalatest.flatspec.AnyFlatSpec


class DspWrapper(k: Int, expW: Int, mantW: Int) extends MXDsp(k, expW, mantW) {
    val exposedAdderOut = expose(adder.out)
    val exposedMaxElemExp = expose(adder.normalizer.maxElementExp)
    val exposedSharedExp = expose(adder.normalizer.outSharedExp)
}

class MXDspTest extends AnyFlatSpec with ChiselScalatestTester {
    "MXDsp" should "add a and b elementwise" in {
        test(new DspWrapper(2, 4, 3)) { dsp =>
            dsp.io.c.sharedExp.poke(0.U)
            dsp.io.c.elems(0).poke("b00111001".U)
            dsp.io.c.elems(1).poke("b00111001".U)
            dsp.io.d.sharedExp.poke(0.U)
            dsp.io.d.elems(0).poke("b00111001".U)
            dsp.io.d.elems(1).poke("b00111001".U)
            dsp.io.adderInASel.poke(1.U)
            dsp.clock.step(1)
            // println(dsp.exposedMaxElemExp.peek())
            // println(dsp.exposedSharedExp.peek())
            dsp.io.addOut.sharedExp.expect(120.U(8.W))
            dsp.io.addOut.elems.expect(Vec.Lit("b01111001".U(8.W), "b01111001".U(8.W)))

        }
    }
}