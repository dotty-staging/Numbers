/*
 *  IntFunctions.scala
 *  (Numbers)
 *
 *  Copyright (c) 2013-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.numbers

/** Common functions for `Int` values. */
object IntFunctions {
  // ---- unary ops ----

  @inline def abs     (a: Int): Int   = math.abs(a)
  @inline def signum  (a: Int): Int   = math.signum(a)
  @inline def squared (a: Int): Long  = { val n = a.toLong; n * n }

  @inline def isPowerOfTwo(a: Int): Boolean = (a & (a-1)) == 0

  @inline def nextPowerOfTwo(a: Int): Int = {
    if (a > 0x40000000) throw new ArithmeticException(s"Integer overflow: nextPowerOfTwo($a)")
    var j = 1
    while (j < a) j <<= 1   // in theory would be faster to do zig-zag search
    j
  }

  @inline def isEven(i: Int): Boolean = i % 2 == 0
  @inline def isOdd (i: Int): Boolean = i % 2 == 1

  // ---- binary ops ----
  @inline def min   (a: Int, b: Int): Int = math.min(a, b)
  @inline def max   (a: Int, b: Int): Int = math.max(a, b)

  @inline def div   (a: Int, b: Int): Int = if (b == 0) a else {
    // No. Fucking. Way.
    if (a < 0) (a + 1) / b - 1
    else a / b
  }

  @inline def gcd   (a: Int, b: Int): Int = {
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

  @inline def lcm (a: Int, b: Int): Int =
    if (a == 0 || b == 0) 0
    else (a * b) / gcd(a, b)

  @inline def clip2 (a: Int, b: Int): Int = clip(a, -b, b)
  @inline def excess(a: Int, b: Int): Int = a - math.max(math.min(a, b), -b)
  @inline def fold2 (a: Int, b: Int): Int = fold(a, -b, b)
  @inline def wrap2 (a: Int, b: Int): Int = wrap(a, -b, b)

  // ---- n-arny ops ----

  @inline def clip(in: Int, low: Int, high: Int): Int = math.max(low, math.min(high, in))

  @inline def fold(in: Int, low: Int, high: Int): Int = {
    val b   = high - low
    val b2  = b + b
    val c0  = mod(in - low, b2)
    val c   = if (c0 > b) b2 - c0 else c0
    c + low
  }

  // handles negative numbers differently than a % b
  @inline def mod(a: Int, b: Int): Int = if (b == 0) 0 else {
    // No. Fucking. Way.
    var in = a
    if (a >= b) {
      in -= b
      if (in < b) return in
    } else if (a < 0) {
      in += b
      if (in >= 0) return in
    } else return in

    val c = in % b
    if (c < 0) c + b else c
  }

  @inline def wrap(in: Int, low: Int, high: Int): Int = mod(in - low, high - low + 1) + low
}