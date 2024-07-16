import chisel3._
import chisel3.util._
import Consts._
import OpType._
import scala.xml.Elem


class MXBundle(k: Int, expW: Int, mantW: Int) extends Bundle {
    val sharedExp = UInt(SHARED_EXP_W.W)
    val elems = Vec(k, UInt((1 + expW + mantW).W))

    def dotProduct(): Bits = {
        val fp32s = VecInit(elems.map{ elem => 
            val float = new FloatWrapper(expW, mantW, EXP_BIAS(expW), elem)
            float.scale(sharedExp).asFP32.asBits
        })

        fp32s.reduceTree{ case (a, b) => 
            val floatA = new FloatWrapper(8, 23, EXP_BIAS(8), a)
            val floatB = new FloatWrapper(8, 23, EXP_BIAS(8), b)
            (floatA + floatB).asBits
        }
    }
}

object MXBundle {
    def apply(k: Int, expW: Int, mantW: Int): MXBundle = {
        val defaultMX = Wire(new MXBundle(k, expW, mantW))
        defaultMX.sharedExp := 0.U
        defaultMX.elems := VecInit(Seq.fill(k)(0.U((expW + mantW).W)))
        defaultMX
    }
}


class ElementwiseOp(k: Int, expW: Int, mantW: Int, op: OpType, normalize: Boolean) extends Module {
    val elemW = 1 + expW + mantW

    val io = IO(new Bundle {
        val a = Input(new MXBundle(k, expW, mantW))
        val b = Input(new MXBundle(k, expW, mantW))
        val scale = if (normalize) None else Some(Input(UInt(SHARED_EXP_W.W)))
        val out = Output(new MXBundle(k, expW, mantW))
    })

    val out = VecInit((io.a.elems zip io.b.elems).map{ case (a, b) => 
        val floatA = new FloatWrapper(expW, mantW, EXP_BIAS(expW), a)
        val floatB = new FloatWrapper(expW, mantW, EXP_BIAS(expW), b)
        
        op match {
            case Add => (floatA.scale(io.a.sharedExp) + floatB.scale(io.b.sharedExp)).asBits
            case Mult => (floatA.scale(io.a.sharedExp) * floatB.scale(io.b.sharedExp)).asBits
            case _ => 0.U((1 + SHARED_EXP_W + mantW).W)
        }
    })

    val normalizer = Module(new MXNormalizer(k, SHARED_EXP_W, expW, mantW, normalize))
    normalizer.io.inElements := out
    (normalizer.io.scale, io.scale) match {
        case (Some(s1), Some(s2)) => s1 := s2
        case _ => 
    }
    
    io.out.sharedExp := normalizer.io.outSharedExp
    io.out.elems := normalizer.io.outElements
}


class MXDsp(k: Int, expW: Int, mantW: Int, normalize: Boolean = true) extends Module {
    val elemW = 1 + expW + mantW

    val io = IO(new Bundle {
        val a = Input(new MXBundle(k, expW, mantW))
        val b = Input(new MXBundle(k, expW, mantW))
        val c = Input(new MXBundle(k, expW, mantW))
        val d = Input(new MXBundle(k, expW, mantW))
        val adderInASel = Input(UInt(2.W))
        val addOutScale = if (normalize) None else Some(Input(UInt(SHARED_EXP_W.W)))
        val multOutScale = if (normalize) None else Some(Input(UInt(SHARED_EXP_W.W)))
        val addOut = Output(new MXBundle(k, expW, mantW))
        val multOut = Output(new MXBundle(k, expW, mantW))
        val dotOut = Output(Bits((DOT_EXP_W + DOT_MANT_W).W))
    })

    val multiplier = Module(new ElementwiseOp(k, expW, mantW, Mult, normalize))
    multiplier.io.a := io.a
    multiplier.io.b := io.b
    (multiplier.io.scale, io.multOutScale) match {
        case (Some(s1), Some(s2)) => s1 := s2
        case _ => require(normalize)
    }
    val multOutReg = RegNext(multiplier.io.out)
    
    val adder = Module(new ElementwiseOp(k, expW, mantW, Add, normalize))
    adder.io.a := MuxLookup(io.adderInASel, MXBundle(k, expW, mantW))(Seq(
        0.U -> multOutReg,
        1.U -> io.d
    ))
    adder.io.b := io.c
    (adder.io.scale, io.addOutScale) match {
        case (Some(s1), Some(s2)) => s1 := s2
        case _ => require(normalize)
    }

    io.addOut := adder.io.out
    io.multOut := multOutReg
    io.dotOut := multOutReg.dotProduct
}