/*
 *  DoubleFunctions2.scala
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

/** Less common functions for `Double` values. */
object DoubleFunctions2 {
  // -------- unary ops --------

  @inline def not       (d: Double): Double = if (d > 0d) 0d else 1d
  @inline def neg       (d: Double): Double = -d

  @inline def cubed     (d: Double): Double = d * d * d
  @inline def reciprocal(d: Double): Double = 1d / d

  @inline def distort   (d: Double): Double = d / (1 + math.abs(d))

  @inline def softClip(d: Double): Double = {
    val abs = math.abs(d)
    if (abs <= 0.5d) d else (abs - 0.25d) / d
  }

  @inline def rectWindow(d: Double): Double = if (d >= 0d && d <= 1d) 1d else 0d

  @inline def hannWindow(d: Double): Double =
    if (d >= 0d && d <= 1d) {
      0.5d - 0.5d * math.cos(d * TwoPi)
    } else 0d

  @inline def welchWindow(d: Double): Double =
    if (d >= 0d && d <= 1d) {
      math.sin(d * math.Pi)
    } else 0d

  @inline def triWindow(d: Double): Double =
    if (d >= 0d && d <= 1d) {
      if (d < 0.5d) 2d * d
      else -2d * d + 2d
    } else 0d

  @inline def ramp      (d: Double): Double = if (d <= 0) 0 else if (d >= 1) 1 else d
  @inline def sCurve    (d: Double): Double = if (d <= 0) 0 else if (d >  1) 1 else d * d * (3 - 2 * d)

  // -------- binary ops --------

  @inline def ring1     (a: Double, b: Double): Double = a * b + a
  @inline def ring2     (a: Double, b: Double): Double = a * b + a + b
  @inline def ring3     (a: Double, b: Double): Double = a * a * b

  @inline def ring4     (a: Double, b: Double): Double = {
    val ab = a * b
    a * ab - b * ab
  }

  @inline def thresh    (a: Double, b: Double): Double = if (a < b) 0d else a
  @inline def amClip    (a: Double, b: Double): Double = if (b <= 0d) 0d else a * b
  @inline def scaleNeg  (a: Double, b: Double): Double = (math.abs(a) - a) * (0.5 * b + 0.5) + a
}
