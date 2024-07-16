object Consts {
    val SHARED_EXP_W = 8
    val DOT_EXP_W = 8
    val DOT_MANT_W = 23

    val EXP_BIAS = Map(
        8 -> 127,
        4 -> 7
    )

    val MAX_NORMAL_MAP = Map(
        (4, 3) -> "b1111110",
        (8, 3) -> "b11111111110",
        (8, 23) -> "b1111111011111111111111111111111"
    )
}

object OpType extends Enumeration {
    type OpType = Value
    val Add, Mult = Value
}

