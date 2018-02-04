/*
 *  RichFloat.scala
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

package de.sciss
package numbers

final class RichFloat(val toFloat: Float)
  extends AnyVal with RichMixins.UnaryFloatOps
  with RichMixins.NAryFloatOps with RichMixins.NAryDoubleOps /* with Ops.NAryGEOps */ { me =>

  import numbers.{FloatFunctions => rf}
  import me.{toFloat => f}

  protected def toDouble: Double = f.toDouble
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
