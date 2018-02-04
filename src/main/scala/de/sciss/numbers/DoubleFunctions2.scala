/*
 *  DoubleFunctions2.scala
 *  (Numbers)
 *
 *  Copyright (c) 2013-2018 Hanns Holger Rutz. All rights reserved.
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

  @inline def neg         (d: Double): Double = -d

  @inline def cubed       (d: Double): Double = d * d * d
  @inline def reciprocal  (d: Double): Double = 1.0 / d

  // -------- binary ops --------

  @inline def ring1     (a: Double, b: Double): Double = a * b + a
  @inline def ring2     (a: Double, b: Double): Double = a * b + a + b
  @inline def ring3     (a: Double, b: Double): Double = a * a * b

  @inline def ring4     (a: Double, b: Double): Double = {
    val ab = a * b
    a * ab - b * ab
  }

  @inline def thresh    (a: Double, b: Double): Double = if (a < b) 0.0 else a
  @inline def amclip    (a: Double, b: Double): Double = a * 0.5 * (b + math.abs(a))
  @inline def scaleneg  (a: Double, b: Double): Double = (math.abs(a) - a) * (0.5 * b + 0.5) + a
}
