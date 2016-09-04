/*
 * RichDouble.scala
 * (Numbers)
 *
 * Copyright (c) 2013-2016 Hanns Holger Rutz. All rights reserved.
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

package de.sciss
package numbers

final class RichDouble(val toDouble: Double)
  extends AnyVal with RichMixins.NAryDoubleOps /* with RichMixins.NAryGEOps */ { me =>

  import numbers.{DoubleFunctions => rd, DoubleFunctions2 => rd2}
  import me.{toDouble => d}

  // protected def cn = Constant(d.toFloat)

  // recover these from scala.runtime.RichDouble
  // def isInfinity    : Boolean = java.lang.Double.isInfinite(d)
  // def isPosInfinity : Boolean = isInfinity && d > 0.0
  // def isNegInfinity : Boolean = isInfinity && d < 0.0

  // unary ops
  //   def unary_- : Double    = -d

  // def abs       : Double = rd.abs       (d)
  // def ceil      : Double = rd.ceil      (d)
  // def floor     : Double = rd.floor     (d)
  def frac      : Double = rd.frac      (d)
  // def signum    : Double = rd.signum    (d)
  def squared   : Double = rd.squared   (d)
  // def cubed     : Double = rd.cubed     (d)
  def sqrt      : Double = rd.sqrt      (d)
  def exp       : Double = rd.exp       (d)
  def reciprocal: Double = rd2.reciprocal(d)
  def midicps   : Double = rd.midicps   (d)
  def cpsmidi   : Double = rd.cpsmidi   (d)
  def midiratio : Double = rd.midiratio (d)
  def ratiomidi : Double = rd.ratiomidi (d)
  def dbamp     : Double = rd.dbamp     (d)
  def ampdb     : Double = rd.ampdb     (d)
  def octcps    : Double = rd.octcps    (d)
  def cpsoct    : Double = rd.cpsoct    (d)
  def log       : Double = rd.log       (d)
  def log2      : Double = rd.log2      (d)
  def log10     : Double = rd.log10     (d)
  def sin       : Double = rd.sin       (d)
  def cos       : Double = rd.cos       (d)
  def tan       : Double = rd.tan       (d)
  def asin      : Double = rd.asin      (d)
  def acos      : Double = rd.acos      (d)
  def atan      : Double = rd.atan      (d)
  def sinh      : Double = rd.sinh      (d)
  def cosh      : Double = rd.cosh      (d)
  def tanh      : Double = rd.tanh      (d)

  //   def distort : Double    = rd.distort( d )
  //   def softclip : Double   = rd.softclip( d )
  //   def ramp : Double       = rd.ramp( d )
  //   def scurve : Double     = rd.scurve( d )
}