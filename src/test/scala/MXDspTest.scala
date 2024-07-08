import chisel3._
import chisel3.experimental.VecLiterals._
import chiseltest._
import chiseltest.experimental.expose
import org.scalatest.flatspec.AnyFlatSpec


class DspWrapper(k: Int, expW: Int, mantW: Int) extends MXDsp(k, expW, mantW) {
    val exposedAdderOut = expose(adder.out)
    val exposedMultOut = expose(multiplier.out)
    val exposedMaxElemExp = expose(adder.normalizer.maxElementExp)
    val exposedSharedExp = expose(adder.normalizer.outSharedExp)
}

class MXDspTest extends AnyFlatSpec with ChiselScalatestTester {
    "MXDsp" should "add numbers with the same exponent" in {
        test(new DspWrapper(2, 4, 3)) { dsp =>
            dsp.io.c.sharedExp.poke(127.U)  // zero after removing bias (127 for 8-bit exponents)
            dsp.io.c.elems(0).poke("b00111001".U)
            dsp.io.c.elems(1).poke("b00111001".U)
            dsp.io.d.sharedExp.poke(127.U)
            dsp.io.d.elems(0).poke("b00111001".U)
            dsp.io.d.elems(1).poke("b00111001".U)
            dsp.io.adderInASel.poke(1.U)    // add C and D
            dsp.clock.step(1)
            dsp.io.addOut.sharedExp.expect(120.U(8.W))
            dsp.io.addOut.elems.expect(Vec.Lit("b01111001".U(8.W), "b01111001".U(8.W)))
        }
    }

    "MXDsp" should "add numbers with different element exponents" in {
        test(new DspWrapper(2, 4, 3)) { dsp =>
            dsp.io.c.sharedExp.poke(127.U)
            dsp.io.c.elems(0).poke("b00111001".U)
            dsp.io.c.elems(1).poke("b00111001".U)
            dsp.io.d.sharedExp.poke(127.U)
            dsp.io.d.elems(0).poke("b01000001".U)
            dsp.io.d.elems(1).poke("b01000001".U)
            dsp.io.adderInASel.poke(1.U)    // add C and D
            dsp.clock.step(1)
            dsp.io.addOut.sharedExp.expect(120.U(8.W))
            dsp.io.addOut.elems.expect(Vec.Lit("b01111101".U(8.W), "b01111101".U(8.W)))
        }
    }

    "MXDsp" should "add numbers with different shared exponents" in {
        test(new DspWrapper(2, 4, 3)) { dsp =>
            dsp.io.c.sharedExp.poke(127.U)
            dsp.io.c.elems(0).poke("b00111001".U)
            dsp.io.c.elems(1).poke("b00111001".U)
            dsp.io.d.sharedExp.poke(128.U)
            dsp.io.d.elems(0).poke("b00111001".U)
            dsp.io.d.elems(1).poke("b00111001".U)
            dsp.io.adderInASel.poke(1.U)    // add C and D
            dsp.clock.step(1)
            dsp.io.addOut.sharedExp.expect(120.U(8.W))
            dsp.io.addOut.elems.expect(Vec.Lit("b01111101".U(8.W), "b01111101".U(8.W)))
        }
    }

    "MXDsp" should "subtract numbers with the same exponent" in {
        test(new DspWrapper(2, 4, 3)) { dsp =>
            dsp.io.c.sharedExp.poke(127.U)
            dsp.io.c.elems(0).poke("b10111010".U)
            dsp.io.c.elems(1).poke("b10111010".U) 
            dsp.io.d.sharedExp.poke(127.U)
            dsp.io.d.elems(0).poke("b00111011".U)
            dsp.io.d.elems(1).poke("b00111011".U)
            dsp.io.adderInASel.poke(1.U)    // D + C
            dsp.clock.step(1)
            dsp.io.addOut.sharedExp.expect(116.U(8.W))
            dsp.io.addOut.elems.expect(Vec.Lit("b01111000".U(8.W), "b01111000".U(8.W)))

            dsp.io.c.sharedExp.poke(127.U)
            dsp.io.c.elems(0).poke("b00111011".U)
            dsp.io.c.elems(1).poke("b00111011".U)
            dsp.io.d.sharedExp.poke(127.U)
            dsp.io.d.elems(0).poke("b10111010".U)
            dsp.io.d.elems(1).poke("b10111010".U)
            dsp.io.adderInASel.poke(1.U)    // D + C
            dsp.clock.step(1)
            dsp.io.addOut.sharedExp.expect(116.U(8.W))
            dsp.io.addOut.elems.expect(Vec.Lit("b01111000".U(8.W), "b01111000".U(8.W)))
        }
    }

    "MXDsp" should "subtract numbers with different element exponents" in {
        test(new DspWrapper(2, 4, 3)) { dsp =>
            dsp.io.c.sharedExp.poke(127.U)
            dsp.io.c.elems(0).poke("b10111001".U)
            dsp.io.c.elems(1).poke("b10111001".U)
            dsp.io.d.sharedExp.poke(127.U)
            dsp.io.d.elems(0).poke("b01000001".U)
            dsp.io.d.elems(1).poke("b01000001".U)
            dsp.io.adderInASel.poke(1.U)    // D + C
            dsp.clock.step(1)
            dsp.io.addOut.sharedExp.expect(119.U(8.W))
            dsp.io.addOut.elems.expect(Vec.Lit("b01111010".U(8.W), "b01111010".U(8.W)))

            dsp.io.c.sharedExp.poke(127.U)
            dsp.io.c.elems(0).poke("b01000001".U)
            dsp.io.c.elems(1).poke("b01000001".U)
            dsp.io.d.sharedExp.poke(127.U)
            dsp.io.d.elems(0).poke("b10111001".U)
            dsp.io.d.elems(1).poke("b10111001".U)
            dsp.io.adderInASel.poke(1.U)    // D + C
            dsp.clock.step(1)
            dsp.io.addOut.sharedExp.expect(119.U(8.W))
            dsp.io.addOut.elems.expect(Vec.Lit("b01111010".U(8.W), "b01111010".U(8.W)))
        }
    }

    "MXDsp" should "subtract numbers with different shared exponents" in {
        test(new DspWrapper(2, 4, 3)) { dsp =>
            dsp.io.c.sharedExp.poke(127.U)
            dsp.io.c.elems(0).poke("b00111001".U)
            dsp.io.c.elems(1).poke("b00111001".U)
            dsp.io.d.sharedExp.poke(128.U)
            dsp.io.d.elems(0).poke("b10111001".U)
            dsp.io.d.elems(1).poke("b10111001".U)
            dsp.io.adderInASel.poke(1.U)    // add C and D
            dsp.clock.step(1)
            dsp.io.addOut.sharedExp.expect(119.U(8.W))
            dsp.io.addOut.elems.expect(Vec.Lit("b11111010".U(8.W), "b11111010".U(8.W)))
        }
    }

    "MXDsp" should "multiply two positive numbers" in {
        test(new DspWrapper(2, 4, 3)) { dsp =>
            /* Multiplication without overflow */ 
            // A = 1.001 * 2^0
            dsp.io.a.sharedExp.poke(127.U)
            dsp.io.a.elems(0).poke("b00111001".U)
            dsp.io.a.elems(1).poke("b00111001".U)
            // B = 1.001 * 2^0
            dsp.io.b.sharedExp.poke(127.U)
            dsp.io.b.elems(0).poke("b00111001".U)
            dsp.io.b.elems(1).poke("b00111001".U)
            dsp.clock.step(1)
            // multOut = 1.010 * 2^0 = 1.010 * 2^8 * 2^-8
            dsp.io.multOut.sharedExp.expect(119.U)
            dsp.io.multOut.elems(0).expect("b01111010".U)
            dsp.io.multOut.elems(1).expect("b01111010".U)

            /* Multiplication with overflow */
            // A = 1.111 * 2^0
            dsp.io.a.sharedExp.poke(127.U)
            dsp.io.a.elems(0).poke("b00111111".U)
            dsp.io.a.elems(1).poke("b00111111".U)
            // B = 1.111 * 2^0
            dsp.io.b.sharedExp.poke(127.U)
            dsp.io.b.elems(0).poke("b00111111".U)
            dsp.io.b.elems(1).poke("b00111111".U)
            dsp.clock.step(1)
            // multOut = 11.10 * 2^0 = 1.110 * 2^1 = 1.110 * 2^8 * 2^-7
            dsp.io.multOut.sharedExp.expect(120.U)
            dsp.io.multOut.elems(0).expect("b01111110".U)
            dsp.io.multOut.elems(1).expect("b01111110".U)
        }
    }

    "MXDsp" should "multiply two negative numbers" in {
        test(new DspWrapper(2, 4, 3)) { dsp =>
            /* Multiplication without overflow */
            // A = -1.001 * 2^0
            dsp.io.a.sharedExp.poke(127.U)
            dsp.io.a.elems(0).poke("b10111001".U)
            dsp.io.a.elems(1).poke("b10111001".U)
            // B = -1.001 * 2^0
            dsp.io.b.sharedExp.poke(127.U)
            dsp.io.b.elems(0).poke("b10111001".U)
            dsp.io.b.elems(1).poke("b10111001".U)
            dsp.clock.step(1)
            // multOut = 1.010 * 2^0 = 1.010 * 2^8 * 2^-8
            dsp.io.multOut.sharedExp.expect(119.U)
            dsp.io.multOut.elems(0).expect("b01111010".U)
            dsp.io.multOut.elems(1).expect("b01111010".U)

            /* Multiplication with overflow */
            // A = -1.111 * 2^0
            dsp.io.a.sharedExp.poke(127.U)
            dsp.io.a.elems(0).poke("b10111111".U)
            dsp.io.a.elems(1).poke("b10111111".U)
            // B = -1.111 * 2^0
            dsp.io.b.sharedExp.poke(127.U)
            dsp.io.b.elems(0).poke("b10111111".U)
            dsp.io.b.elems(1).poke("b10111111".U)
            dsp.clock.step(1)
            // multOut = 11.10 * 2^0 = 1.110 * 2^1 = 1.110 * 2^8 * 2^-7
            dsp.io.multOut.sharedExp.expect(120.U)
            dsp.io.multOut.elems(0).expect("b01111110".U)
            dsp.io.multOut.elems(1).expect("b01111110".U)
        }
    }

    "MXDsp" should "multiply a positive number with a negative number" in {
        test(new DspWrapper(2, 4, 3)) { dsp =>
            /* Multiplication without overflow */
            // A = 1.001 * 2^0
            dsp.io.a.sharedExp.poke(127.U)
            dsp.io.a.elems(0).poke("b00111001".U)
            dsp.io.a.elems(1).poke("b00111001".U)
            // B = -1.001 * 2^0
            dsp.io.b.sharedExp.poke(127.U)
            dsp.io.b.elems(0).poke("b10111001".U)
            dsp.io.b.elems(1).poke("b10111001".U)
            dsp.clock.step(1)
            // multOut = -1.010 * 2^0 = -1.010 * 2^8 * 2^-8
            dsp.io.multOut.sharedExp.expect(119.U)
            dsp.io.multOut.elems(0).expect("b11111010".U)
            dsp.io.multOut.elems(1).expect("b11111010".U)

            /* Multiplication with overflow */
            // A = 1.111 * 2^0
            dsp.io.a.sharedExp.poke(127.U)
            dsp.io.a.elems(0).poke("b00111111".U)
            dsp.io.a.elems(1).poke("b00111111".U)
            // B = -1.111 * 2^0
            dsp.io.b.sharedExp.poke(127.U)
            dsp.io.b.elems(0).poke("b10111111".U)
            dsp.io.b.elems(1).poke("b10111111".U)
            dsp.clock.step(1)
            // multOut = -11.10 * 2^0 = -1.110 * 2^1 = -1.110 * 2^8 * 2^-7
            dsp.io.multOut.sharedExp.expect(120.U)
            dsp.io.multOut.elems(0).expect("b11111110".U)
            dsp.io.multOut.elems(1).expect("b11111110".U)
        }
    }
}