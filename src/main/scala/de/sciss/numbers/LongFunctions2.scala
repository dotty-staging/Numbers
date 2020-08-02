/*
 *  LongFunctions2.scala
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

import de.sciss.numbers.{LongFunctions => rl}

/** Less common functions for `Long` values. */
object LongFunctions2 {
  @inline def cubed   (a: Long): Long  = a * a * a

  @inline def difSqr  (a: Long, b: Long): Long = a * a - b * b
  @inline def sumSqr  (a: Long, b: Long): Long = a * a + b * b

  @inline def sqrSum  (a: Long, b: Long): Long = {
    val z = a + b
    z * z
  }

  @inline def sqrDif  (a: Long, b: Long): Long = {
    val z = a - b
    z * z
  }

  @inline def absDif    (a: Long, b: Long): Long = math.abs(a - b) // XXX TODO -- may overflow

  @inline def roundTo   (a: Long, b: Long): Long = if (b == 0) a else rl.div(a + b/2, b) * b
  @inline def roundUpTo (a: Long, b: Long): Long = if (b == 0) a else rl.div(a + b-1, b) * b
  @inline def trunc     (a: Long, b: Long): Long = if (b == 0) a else rl.div(a      , b) * b
}
