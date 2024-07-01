import chisel3._
import chisel3.util._

class ElementwiseMult(k: Int, expW: Int, mantW: Int) extends Module {
    val elementW = 1 + expW + mantW

    val io = IO(new Bundle {
        val sharedExpA = Input(UInt(Consts.SHARED_EXP_W.W))
        val elementsA = Input(Vec(k, Bits(elementW.W)))
        val sharedExpB = Input(UInt(Consts.SHARED_EXP_W.W))
        val elementsB = Input(Vec(k, Bits(elementW.W)))
        val sharedExpOut = Output(UInt(Consts.SHARED_EXP_W.W))
        val elementsOut = Output(Vec(k, Bits(elementW.W)))
    })

    val out = VecInit((io.elementsA zip io.elementsB).map{ case (a, b) => 
        val floatA = new FloatWrapper(a, expW, mantW)
        val floatB = new FloatWrapper(b, expW, mantW)
        (floatA.scale(io.sharedExpA) * floatB.scale(io.sharedExpB)).data
    })

    val normalizer = Module(new MXNormalizer(k, Consts.SHARED_EXP_W, expW, mantW))
    normalizer.io.inElements := out
    
    io.sharedExpOut := normalizer.io.outSharedExp
    io.elementsOut := normalizer.io.outElements
}