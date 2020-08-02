/*
 *  FloatFunctions2.scala
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

/** Less common functions for `Float` values. */
object FloatFunctions2 {
  // -------- unary ops --------

  @inline def not       (f: Float): Float = if (f > 0f) 0f else 1f
  @inline def neg       (f: Float): Float = -f

  @inline def cubed     (f: Float): Float = f * f * f
  @inline def reciprocal(f: Float): Float = 1f / f

  @inline def distort   (f: Float): Float = f / (1 + math.abs(f))

  @inline def softClip(f: Float): Float = {
    val abs = math.abs(f)
    if (abs <= 0.5f) f else (abs - 0.25f) / f
  }

  @inline def rectWindow(f: Float): Float = if (f >= 0f && f <= 1f) 1f else 0f

  @inline def hannWindow(f: Float): Float =
    if (f >= 0f && f <= 1f) {
      0.5f - 0.5f * math.cos(f * TwoPi).toFloat
    } else 0f

  @inline def welchWindow(f: Float): Float =
    if (f >= 0f && f <= 1f) {
      math.sin(f * math.Pi).toFloat
    } else 0f

  @inline def triWindow(f: Float): Float =
    if (f >= 0f && f <= 1f) {
      if (f < 0.5f) 2f * f
      else -2f * f + 2f
    } else 0f

  @inline def ramp      (f: Float): Float = if (f <= 0) 0 else if (f >= 1) 1 else f
  @inline def sCurve    (f: Float): Float = if (f <= 0) 0 else if (f >  1) 1 else f * f * (3 - 2 * f)

  // -------- binary ops --------

  @inline def ring1   (a: Float, b: Float): Float = a * b + a
  @inline def ring2   (a: Float, b: Float): Float = a * b + a + b
  @inline def ring3   (a: Float, b: Float): Float = a * a * b

  @inline def ring4   (a: Float, b: Float): Float = {
    val ab = a * b
    a * ab - b * ab
  }

  @inline def thresh  (a: Float, b: Float): Float = if (a < b) 0f else a
  @inline def amClip  (a: Float, b: Float): Float = if (b <= 0f) 0f else a * b
  @inline def scaleNeg(a: Float, b: Float): Float = (math.abs(a) - a) * (0.5f * b + 0.5f) + a
}
