/*
 * RichMixins.scala
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

object RichMixins {
  trait UnaryFloatOps extends Any { me =>
    import numbers.{FloatFunctions => rf, FloatFunctions2 => rf2}

    protected def toFloat: Float

    import me.{toFloat => f}

    // unary ops
    def sqrt      : Float = rf.sqrt       (f)
    def exp       : Float = rf.exp        (f)
    def reciprocal: Float = rf2.reciprocal(f)
    def midicps   : Float = rf.midicps    (f)
    def cpsmidi   : Float = rf.cpsmidi    (f)
    def midiratio : Float = rf.midiratio  (f)
    def ratiomidi : Float = rf.ratiomidi  (f)
    def dbamp     : Float = rf.dbamp      (f)
    def ampdb     : Float = rf.ampdb      (f)
    def octcps    : Float = rf.octcps     (f)
    def cpsoct    : Float = rf.cpsoct     (f)
    def log       : Float = rf.log        (f)
    def log2      : Float = rf.log2       (f)
    def log10     : Float = rf.log10      (f)
    def sin       : Float = rf.sin        (f)
    def cos       : Float = rf.cos        (f)
    def tan       : Float = rf.tan        (f)
    def asin      : Float = rf.asin       (f)
    def acos      : Float = rf.acos       (f)
    def atan      : Float = rf.atan       (f)
    def sinh      : Float = rf.sinh       (f)
    def cosh      : Float = rf.cosh       (f)
    def tanh      : Float = rf.tanh       (f)

    // def distort : Float     = f / (1 + math.abs( f ))
    // def softclip : Float    = { val absx = math.abs( f ); if( absx <= 0.5f ) f else (absx - 0.25f) / f}
    // def ramp : Float        = if( f <= 0 ) 0 else if( f >= 1 ) 1 else f
    // def scurve : Float      = if( f <= 0 ) 0 else if( f > 1 ) 1 else f * f * (3 - 2 * f)
  }

  trait NAryFloatOps extends Any { me =>
    import numbers.{FloatFunctions => rf}

    protected def toFloat: Float

    import me.{toFloat => f}

    // -------- binary ops --------

    // def +( b: Float ) : Float        = rf.+( f, b )
    // def -( b: Float ) : Float        = rf.-( f, b )
    // def *( b: Float ) : Float        = rf.*( f, b )

    def div     (b: Float): Int   = rf.div      (f, b)

    // def /( b: Float ) : Float        = rf./( f, b )
    // def %( b: Float ) : Float        = rf.%( f, b )
    // def sig_==( b: Float ) : Int        = rf.sig_==( f, b )
    // def sig_!=( b: Float ) : Int        = rf.sig_!=( f, b )
    // def <( b: Float ) : Float	      = rf.<( f, b )
    // def >( b: Float ) : Float	      = rf.>( f, b )
    // def <=( b: Float ) : Float	      = rf.<=( f, b )
    // def >=( b: Float ) : Float	      = rf.>=( f, b )

    // def min     (b: Float): Float = rf.min      (f, b)
    // def max     (b: Float): Float = rf.max      (f, b)

    // def &( b: Float ) : Float	      = rf.&( f, b )
    // def |( b: Float ) : Float	      = rf.|( f, b )
    // def ^( b: Float ) : Float	      = rf.^( f, b )

    def roundTo  (b: Float): Float = rf.roundTo  (f, b)
    def roundUpTo(b: Float): Float = rf.roundUpTo(f, b)
    def trunc   (b: Float): Float = rf.trunc    (f, b)
    def atan2   (b: Float): Float = rf.atan2    (f, b)
    def hypot   (b: Float): Float = rf.hypot    (f, b)
    def hypotx  (b: Float): Float = rf.hypotx   (f, b)
    def pow     (b: Float): Float = rf.pow      (f, b)
    // def ring1   (b: Float): Float = rf.ring1    (f, b)
    // def ring2   (b: Float): Float = rf.ring2    (f, b)
    // def ring3   (b: Float): Float = rf.ring3    (f, b)
    // def ring4   (b: Float): Float = rf.ring4    (f, b)
    def difsqr  (b: Float): Float = rf.difsqr   (f, b)
    def sumsqr  (b: Float): Float = rf.sumsqr   (f, b)
    def sqrsum  (b: Float): Float = rf.sqrsum   (f, b)
    def sqrdif  (b: Float): Float = rf.sqrdif   (f, b)
    def absdif  (b: Float): Float = rf.absdif   (f, b)
    // def thresh  (b: Float): Float = rf.thresh   (f, b)
    // def amclip  (b: Float): Float = rf.amclip   (f, b)
    // def scaleneg(b: Float): Float = rf.scaleneg (f, b)
    def clip2   (b: Float): Float = rf.clip2    (f, b)
    def excess  (b: Float): Float = rf.excess   (f, b)
    def fold2   (b: Float): Float = rf.fold2    (f, b)
    def wrap2   (b: Float): Float = rf.wrap2    (f, b)

    // def firstarg( b: Float ) : Float = d
    
    def clip    (low: Float, high: Float): Float = rf.clip(f, low, high)
    def fold    (low: Float, high: Float): Float = rf.fold(f, low, high)
    def wrap    (low: Float, high: Float): Float = rf.wrap(f, low, high)

    def linlin(inLow: Float, inHigh: Float, outLow: Float, outHigh: Float): Float =
      rf.linlin(f, inLow, inHigh, outLow, outHigh)

    def linexp(inLow: Float, inHigh: Float, outLow: Float, outHigh: Float): Float =
      rf.linexp(f, inLow, inHigh, outLow, outHigh)

    def explin(inLow: Float, inHigh: Float, outLow: Float, outHigh: Float): Float =
      rf.explin(f, inLow, inHigh, outLow, outHigh)

    def expexp(inLow: Float, inHigh: Float, outLow: Float, outHigh: Float): Float =
      rf.explin(f, inLow, inHigh, outLow, outHigh)
  }

  trait NAryDoubleOps extends Any { me =>
    import numbers.{DoubleFunctions => rd}

    protected def toDouble: Double

    import me.{toDouble => d}

    // recover these from scala.runtime.RichDouble
    //    def until(end: Double): Range.Partial[Double, NumericRange[Double]] =
    //      new Range.Partial[Double, NumericRange[Double]](until(end, _))
    //
    //    def until(end: Double, step: Double): NumericRange [Double] = Range.Double(d, end, step)
    //
    //    def to(end: Double): Range.Partial[Double, NumericRange[Double]] =
    //      new Range.Partial[Double, NumericRange[Double]](to(end, _))
    //
    //    def to(end: Double, step: Double): NumericRange [Double] = Range.Double.inclusive(d, end, step)

    // binary ops
    // def min     (b: Double): Double = rd.min      (d, b)
    // def max     (b: Double): Double = rd.max      (d, b)
    def roundTo  (b: Double): Double = rd.roundTo  (d, b)
    def roundUpTo(b: Double): Double = rd.roundUpTo(d, b)
    def trunc   (b: Double): Double = rd.trunc    (d, b)
    def atan2   (b: Double): Double = rd.atan2    (d, b)
    def hypot   (b: Double): Double = rd.hypot    (d, b)
    def hypotx  (b: Double): Double = rd.hypotx   (d, b)
    def pow     (b: Double): Double = rd.pow      (d, b)
    // def ring1   (b: Double): Double = rd.ring1    (d, b)
    // def ring2   (b: Double): Double = rd.ring2    (d, b)
    // def ring3   (b: Double): Double = rd.ring3    (d, b)
    // def ring4   (b: Double): Double = rd.ring4    (d, b)
    def difsqr  (b: Double): Double = rd.difsqr   (d, b)
    def sumsqr  (b: Double): Double = rd.sumsqr   (d, b)
    def sqrsum  (b: Double): Double = rd.sqrsum   (d, b)
    def sqrdif  (b: Double): Double = rd.sqrdif   (d, b)
    def absdif  (b: Double): Double = rd.absdif   (d, b)
    // def thresh  (b: Double): Double = rd.thresh   (d, b)
    // def amclip  (b: Double): Double = rd.amclip   (d, b)
    // def scaleneg(b: Double): Double = rd.scaleneg (d, b)
    def clip2   (b: Double): Double = rd.clip2    (d, b)
    def excess  (b: Double): Double = rd.excess   (d, b)
    def fold2   (b: Double): Double = rd.fold2    (d, b)
    def wrap2   (b: Double): Double = rd.wrap2    (d, b)

    def clip    (low: Double, high: Double): Double = rd.clip(d, low, high)
    def fold    (low: Double, high: Double): Double = rd.fold(d, low, high)
    def wrap    (low: Double, high: Double): Double = rd.wrap(d, low, high)

    // def firstarg( b: Double ) : Double  = d

    def linlin(inLow: Double, inHigh: Double, outLow: Double, outHigh: Double): Double =
      rd.linlin(d, inLow, inHigh, outLow, outHigh)

    def linexp(inLow: Double, inHigh: Double, outLow: Double, outHigh: Double): Double =
      rd.linexp(d, inLow, inHigh, outLow, outHigh)

    def explin(inLow: Double, inHigh: Double, outLow: Double, outHigh: Double): Double =
      rd.explin(d, inLow, inHigh, outLow, outHigh)

    def expexp(inLow: Double, inHigh: Double, outLow: Double, outHigh: Double): Double =
      rd.expexp(d, inLow, inHigh, outLow, outHigh)
  }

  //   sealed trait NAryDoubleOps2 extends NAryDoubleOps {
  //      def round( b: Double ) : Double     = rd.round( d, b )
  //      def roundup( b: Double ) : Double   = rd.roundup( d, b )
  //      def trunc( b: Double ) : Double     = rd.trunc( d, b )
  //   }
}
