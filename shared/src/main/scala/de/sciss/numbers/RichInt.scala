/*
 *  RichInt.scala
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

package de.sciss
package numbers

final class RichInt(val toInt: Int)
  extends AnyVal with RichMixins.UnaryFloatOps with RichMixins.NAryFloatOps
  with RichMixins.NAryDoubleOps /* with Ops.NAryGEOps */ { me =>

  import numbers.{IntFunctions => ri, IntFunctions2 => ri2}
  import me.{toInt => i}

  protected def toFloat : Float   = i.toFloat
  protected def toDouble: Double  = i.toDouble
  // protected def cn  = Constant(i.toFloat)

  // recover these from scala.runtime.RichFloat
  //   def isInfinity: Boolean = java.lang.Float.isInfinite( x )
  //   def isPosInfinity: Boolean = isInfinity && x > 0.0
  //   def isNegInfinity: Boolean = isInfinity && x < 0.0

  // more unary ops
  // def unary_- : Int       = -i

  // already covered by standard Scala wrapper
  // def abs     : Int   = ri.abs    (i)

  //   def ceil : Float	      = math.ceil( i ).toFloat
  //   def floor : Float	      = math.floor( i ).toFloat
  //   def frac : Float	      = rf.frac( i )

  // already covered by standard Scala wrapper
  // def signum  : Int   = ri.signum (i)

  def squared : Long  = ri .squared(i)
  def cubed   : Long  = ri2.cubed  (i)

  def isPowerOfTwo  : Boolean = ri.isPowerOfTwo   (i)
  def nextPowerOfTwo: Int     = ri.nextPowerOfTwo (i)
  def isEven        : Boolean = ri.isEven         (i)
  def isOdd         : Boolean = ri.isOdd          (i)

  // more binary ops

  // already covered by standard Scala wrapper
  // def min(b: Int): Int = ri.min(i, b)
  // def max(b: Int): Int = ri.max(i, b)

  def gcd (b: Int): Int = ri.gcd(i, b)
  def lcm (b: Int): Int = ri.lcm(i, b)

  // def until(end: Int           ): Range = Range(i, end      )
  // def until(end: Int, step: Int): Range = Range(i, end, step)

  // def to(end: Int           ): Range.Inclusive = Range.inclusive(i, end      )
  // def to(end: Int, step: Int): Range.Inclusive = Range.inclusive(i, end, step)

  // def toBinaryString: String  = java.lang.Integer.toBinaryString(i)
  // def toHexString   : String  = java.lang.Integer.toHexString   (i)
  // def toOctalString : String  = java.lang.Integer.toOctalString (i)

  ////////

  def difSqr    (b: Int): Long  = ri2.difSqr    (i, b)
  def sumSqr    (b: Int): Long  = ri2.sumSqr    (i, b)
  def sqrSum    (b: Int): Long  = ri2.sqrSum    (i, b)
  def sqrDif    (b: Int): Long  = ri2.sqrDif    (i, b)
  def absDif    (b: Int): Int   = ri2.absDif    (i, b)

  def roundTo   (b: Int): Int   = ri2.roundTo   (i, b)
  def roundUpTo (b: Int): Int   = ri2.roundUpTo (i, b)
  def trunc     (b: Int): Int   = ri2.trunc     (i, b)

  def clip2     (b: Int): Int   = ri.clip2      (i, b)
  def excess    (b: Int): Int   = ri.excess     (i, b)
  def fold2     (b: Int): Int   = ri.fold2      (i, b)
  def wrap2     (b: Int): Int   = ri.wrap2      (i, b)

  def mod       (b: Int): Int   = ri.mod        (i, b)

  def clip    (low: Int, high: Int): Int = ri.clip(i, low, high)
  def fold    (low: Int, high: Int): Int = ri.fold(i, low, high)
  def wrap    (low: Int, high: Int): Int = ri.wrap(i, low, high)
}