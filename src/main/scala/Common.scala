object Consts {
    val SHARED_EXP_W = 8
    val SHARED_EXP_BIAS = 127
    val DOT_EXP_W = 8
    val DOT_MANT_W = 23
}

object OpType extends Enumeration {
    type OpType = Value
    val Add, Mult = Value
}

