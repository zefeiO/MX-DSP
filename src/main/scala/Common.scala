object Consts {
    val SHARED_EXP_W = 8
    val DOT_EXP_W = 8
    val DOT_MANT_W = 23

    val EXP_BIAS = Map(
        8 -> 127,
        4 -> 7
    )
}

object OpType extends Enumeration {
    type OpType = Value
    val Add, Mult = Value
}

