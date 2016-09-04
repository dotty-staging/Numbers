package de.sciss.numbers

object LongFunctions {
  // ---- binary ops ----
  @inline def gcd (a: Long, b: Long): Long = {
    if (a == 0) return b
    if (b == 0) return a

    var av = math.abs(a)
    var bv = math.abs(b)
    val isNeg = a <= 0 && b <= 0

    if (av == 1 || bv == 1)
      return if (isNeg) -1 else 1

    if (av < bv) {
      val tmp = av
      av = bv
      bv = tmp
    }
    while (bv > 0) {
      val tmp = av % bv
      av = bv
      bv = tmp
    }

    if (isNeg) 0 - av else av
  }

  @inline def lcm (a: Long, b: Long): Long =
    if (a == 0 || b == 0) 0
    else (a * b) / gcd(a, b)
}
