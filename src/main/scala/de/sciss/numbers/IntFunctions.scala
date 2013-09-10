/*
 * IntFunctions.scala
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

/** Common functions for `Int` values. */
object IntFunctions {
  @inline def abs     (a: Int): Int   = math.abs(a)
  @inline def signum  (a: Int): Int   = math.signum(a)
  @inline def squared (a: Int): Long  = { val n = a.toLong; n * n }

  @inline def isPowerOfTwo(a: Int): Boolean = (a & (a-1)) == 0

  @inline def nextPowerOfTwo(a: Int): Int = {
    if (a > 0x40000000) throw new IllegalArgumentException(s"Integer overflow: nextPowerOfTwo($a)")
 		var j = 1
    while (j < a) j <<= 1   // in theory would be faster to do zig-zag search
 		j
 	}

  // ---- binary ops ----
  @inline def min   (a: Int, b: Int): Int = math.min(a, b)
  @inline def max   (a: Int, b: Int): Int = math.max(a, b)

  @inline def clip2 (a: Int, b: Int): Int = clip(a, -b, b)
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
  @inline def mod(a: Int, b: Int): Int =
    if (a >= 0) a % b else {
      val c = -a % b
      if (c == 0) 0 else b - c
    }

  @inline def wrap(in: Int, low: Int, high: Int): Int = mod(in - low, high - low + 1) + low
}