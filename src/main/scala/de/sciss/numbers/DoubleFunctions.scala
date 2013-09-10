/*
 * DoubleFunctions.scala
 * (Numbers)
 *
 * Copyright (c) 2013 Hanns Holger Rutz. All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *
 * For further information, please contact Hanns Holger Rutz at
 * contact@sciss.de
 */

package de.sciss.numbers

/** Common functions for `Double` values. */
object DoubleFunctions {
   // -------- unary ops --------

  @inline def abs         (d: Double): Double = math.abs(d)
  @inline def ceil        (d: Double): Double = math.ceil(d)
  @inline def floor       (d: Double): Double = math.floor(d)
  @inline def frac        (d: Double): Double = d - math.floor(d)

  // according to jmc
  @inline def signum      (d: Double): Double = math.signum(d)
  @inline def squared     (d: Double): Double = d * d
  @inline def sqrt        (d: Double): Double = math.sqrt(d)
  @inline def exp         (d: Double): Double = math.exp(d)
  @inline def midicps     (d: Double): Double = 440 * math.pow(2, (d - 69) / 12)
  @inline def cpsmidi     (d: Double): Double = math.log(d / 440) / Log2 * 12 + 69
  @inline def midiratio   (d: Double): Double = math.pow(2, d / 12)
  @inline def ratiomidi   (d: Double): Double = 12 * math.log(d) / Log2
  @inline def dbamp       (d: Double): Double = math.pow(10, d * 0.05)
  @inline def ampdb       (d: Double): Double = math.log10(d) * 20
  @inline def octcps      (d: Double): Double = 440 * math.pow(2, d - 4.75)
  @inline def cpsoct      (d: Double): Double = math.log(d / 440) / Log2 + 4.75
  @inline def log         (d: Double): Double = math.log(d)
  @inline def log2        (d: Double): Double = math.log(d) / Log2
  @inline def log10       (d: Double): Double = math.log10(d)
  @inline def sin         (d: Double): Double = math.sin(d)
  @inline def cos         (d: Double): Double = math.cos(d)
  @inline def tan         (d: Double): Double = math.tan(d)
  @inline def asin        (d: Double): Double = math.asin(d)
  @inline def acos        (d: Double): Double = math.acos(d)
  @inline def atan        (d: Double): Double = math.atan(d)
  @inline def sinh        (d: Double): Double = math.sinh(d)
  @inline def cosh        (d: Double): Double = math.cosh(d)
  @inline def tanh        (d: Double): Double = math.tanh(d)

  //   @inline def distort( d: Double ) : Double    = d / (1 + math.abs( d ))
  //   @inline def softclip( d: Double ) : Double   = { val absx = math.abs( d ); if( absx <= 0.5 ) d else (absx - 0.25) / d}
  //   @inline def ramp( d: Double ) : Double       = if( d <= 0 ) 0 else if( d >= 1 ) 1 else d
  //   @inline def scurve( d: Double ) : Double     = if( d <= 0 ) 0 else if( d > 1 ) 1 else d * d * (3 - 2 * d)

  // ---- binary ops ----

  @inline def +         (a: Double, b: Double): Double = a + b
  @inline def -         (a: Double, b: Double): Double = a - b
  @inline def *         (a: Double, b: Double): Double = a * b
  @inline def div       (a: Double, b: Double): Int    = (a / b).toInt
  @inline def /         (a: Double, b: Double): Double = a / b
  @inline def %         (a: Double, b: Double): Double = a % b

  //   @inline def sig_==
  //   @inline def sig_!=
  @inline def <         (a: Double, b: Double): Boolean = a < b
  @inline def >         (a: Double, b: Double): Boolean = a > b
  @inline def <=        (a: Double, b: Double): Boolean = a <= b
  @inline def >=        (a: Double, b: Double): Boolean = a >= b
  @inline def min       (a: Double, b: Double): Double = math.min(a, b)
  @inline def max       (a: Double, b: Double): Double = math.max(a, b)

  @inline def roundTo   (a: Double, b: Double): Double = if (b == 0) a else math.floor(a / b + 0.5) * b
  @inline def roundUpTo (a: Double, b: Double): Double = if (b == 0) a else math.ceil (a / b) * b
  @inline def trunc     (a: Double, b: Double): Double = if (b == 0) a else math.floor(a / b) * b
  @inline def atan2     (a: Double, b: Double): Double = math.atan2(a, b)
  @inline def hypot     (a: Double, b: Double): Double = math.hypot(a, b)

  @inline def hypotx    (a: Double, b: Double): Double = {
    val minab = math.min(math.abs(a), math.abs(b))
    a + b - (Sqrt2 - 1) * minab
  }

  @inline def pow       (a: Double, b: Double): Double = math.pow(a, b)

  // <<
  // >>
  // UnsgnRghtShft
  // fill

  @inline def difsqr    (a: Double, b: Double): Double = a * a - b * b
  @inline def sumsqr    (a: Double, b: Double): Double = a * a + b * b

  @inline def sqrsum    (a: Double, b: Double): Double = {
    val z = a + b
    z * z
  }

  @inline def sqrdif    (a: Double, b: Double): Double = {
    val z = a - b
    z * z
  }

  @inline def absdif    (a: Double, b: Double): Double = math.abs(a - b)
  @inline def clip2     (a: Double, b: Double): Double = math.max(math.min(a, b), -b)
  @inline def excess    (a: Double, b: Double): Double = a - math.max(math.min(a, b), -b)
  @inline def fold2     (a: Double, b: Double): Double = fold(a, -b, b)
  @inline def wrap2     (a: Double, b: Double): Double = wrap(a, -b, b)

  // ---- n-ary ops ----

  @inline def clip(in: Double, low: Double, high: Double): Double = ???

  @inline def fold(in: Double, low: Double, high: Double): Double = {
    val x = in - low
    // avoid the divide if possible
    if (in >= high) {
      val f = high + high - in
      if (f >= low) return f
    } else if (in < low) {
      val f = low + low - in
      if (f < high) return f
    } else return in

    if (high == low) return low
    // ok do the divide
    val range = high - low
    val range2 = range + range
    val c = x - range2 * math.floor(x / range2)
    low + (if (c >= range) range2 - c else c)
  }

  @inline def wrap(in: Double, low: Double, high: Double): Double =
    if (in >= high) { // avoid the divide if possible
      val range = high - low
      val in2 = in - range
      if (in2 < high) in2
      else if (high == low) low
      else {
        in2 - range * math.floor((in2 - low) / range)
      }
    } else if (in < low) {
      val range = high - low
      val in2 = in + range
      if (in2 >= low) in2
      else if (high == low) low
      else {
        in2 - range * math.floor((in2 - low) / range)
      }
    } else in

  @inline def linlin(in: Double, inLow: Double, inHigh: Double, outLow: Double, outHigh: Double): Double =
    (in - inLow) / (inHigh - inLow) * (outHigh - outLow) + outLow

  @inline def linexp(in: Double, inLow: Double, inHigh: Double, outLow: Double, outHigh: Double): Double =
    math.pow(outHigh / outLow, (in - inLow) / (inHigh - inLow)) * outLow

  @inline def explin(in: Double, inLow: Double, inHigh: Double, outLow: Double, outHigh: Double): Double =
 		math.log(in / inLow) / math.log(inHigh / inLow) * (outHigh - outLow) + outLow

  @inline def expexp(in: Double, inLow: Double, inHigh: Double, outLow: Double, outHigh: Double): Double = ???
}