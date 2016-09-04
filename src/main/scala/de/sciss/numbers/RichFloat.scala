/*
 * RichFloat.scala
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

final class RichFloat(val toFloat: Float)
  extends AnyVal with RichMixins.UnaryFloatOps
  with RichMixins.NAryFloatOps with RichMixins.NAryDoubleOps /* with Ops.NAryGEOps */ { me =>

  import numbers.{FloatFunctions => rf}
  import me.{toFloat => f}

  protected def toDouble = f.toDouble
  // protected def cn = Constant(f)

  // recover these from scala.runtime.RichFloat
  // def isInfinity    : Boolean = java.lang.Float.isInfinite(f)
  // def isPosInfinity : Boolean = isInfinity && f > 0.0
  // def isNegInfinity : Boolean = isInfinity && f < 0.0

  // more unary ops
  // def unary_- : Float     = -f

  // def abs     : Float = rf.abs    (f)
  // def ceil    : Float = rf.ceil   (f)
  // def floor   : Float = rf.floor  (f)
  def frac    : Float = rf.frac   (f)
  // def signum  : Float = rf.signum (f)
  def squared : Float = rf.squared(f)
  // def cubed   : Float = rf.cubed  (f)

  // more binary ops
  //   def round( b: Float ) : Float    = rf.round( f, b )
  //   def roundup( b: Float ) : Float  = rf.roundup( f, b )
  //   def trunc( b: Float ) : Float    = rf.trunc( f, b )
}
