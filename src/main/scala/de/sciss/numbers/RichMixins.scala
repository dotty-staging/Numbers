/*
 *  RichMixins.scala
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

object RichMixins {
  trait UnaryFloatOps extends Any { me =>
    import numbers.{FloatFunctions => rf, FloatFunctions2 => rf2}

    protected def toFloat: Float

    import me.{toFloat => f}

    // unary ops
    def sqrt      : Float = rf.sqrt       (f)
    def exp       : Float = rf.exp        (f)
    def reciprocal: Float = rf2.reciprocal(f)
    def midiCps   : Float = rf.midiCps    (f)
    def cpsMidi   : Float = rf.cpsMidi    (f)
    def midiRatio : Float = rf.midiRatio  (f)
    def ratioMidi : Float = rf.ratioMidi  (f)
    def dbAmp     : Float = rf.dbAmp      (f)
    def ampDb     : Float = rf.ampDb      (f)
    def octCps    : Float = rf.octCps     (f)
    def cpsOct    : Float = rf.cpsOct     (f)
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

    def rectWindow : Float  = rf2.rectWindow  (f)
    def hannWindow : Float  = rf2.hannWindow  (f)
    def welchWindow: Float  = rf2.welchWindow (f)
    def triWindow  : Float  = rf2.triWindow   (f)
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
    def hypotApx(b: Float): Float = rf.hypotApx (f, b)
    def pow     (b: Float): Float = rf.pow      (f, b)
    // def ring1   (b: Float): Float = rf.ring1    (f, b)
    // def ring2   (b: Float): Float = rf.ring2    (f, b)
    // def ring3   (b: Float): Float = rf.ring3    (f, b)
    // def ring4   (b: Float): Float = rf.ring4    (f, b)
    def difSqr  (b: Float): Float = rf.difSqr   (f, b)
    def sumSqr  (b: Float): Float = rf.sumSqr   (f, b)
    def sqrSum  (b: Float): Float = rf.sqrSum   (f, b)
    def sqrDif  (b: Float): Float = rf.sqrDif   (f, b)
    def absDif  (b: Float): Float = rf.absDif   (f, b)
    // def thresh  (b: Float): Float = rf.thresh   (f, b)
    // def amclip  (b: Float): Float = rf.amclip   (f, b)
    // def scaleneg(b: Float): Float = rf.scaleneg (f, b)
    def clip2   (b: Float): Float = rf.clip2    (f, b)
    def excess  (b: Float): Float = rf.excess   (f, b)
    def fold2   (b: Float): Float = rf.fold2    (f, b)
    def wrap2   (b: Float): Float = rf.wrap2    (f, b)

    def mod     (b: Float): Float = rf.mod      (f, b)

    // def firstarg( b: Float ) : Float = d
    
    def clip    (low: Float, high: Float): Float = rf.clip(f, low, high)
    def fold    (low: Float, high: Float): Float = rf.fold(f, low, high)
    def wrap    (low: Float, high: Float): Float = rf.wrap(f, low, high)

    def linLin(inLow: Float, inHigh: Float, outLow: Float, outHigh: Float): Float =
      rf.linLin(f, inLow, inHigh, outLow, outHigh)

    def linExp(inLow: Float, inHigh: Float, outLow: Float, outHigh: Float): Float =
      rf.linExp(f, inLow, inHigh, outLow, outHigh)

    def expLin(inLow: Float, inHigh: Float, outLow: Float, outHigh: Float): Float =
      rf.expLin(f, inLow, inHigh, outLow, outHigh)

    def expExp(inLow: Float, inHigh: Float, outLow: Float, outHigh: Float): Float =
      rf.expLin(f, inLow, inHigh, outLow, outHigh)
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
    def hypotApx(b: Double): Double = rd.hypotApx (d, b)
    def pow     (b: Double): Double = rd.pow      (d, b)
    // def ring1   (b: Double): Double = rd.ring1    (d, b)
    // def ring2   (b: Double): Double = rd.ring2    (d, b)
    // def ring3   (b: Double): Double = rd.ring3    (d, b)
    // def ring4   (b: Double): Double = rd.ring4    (d, b)
    def difSqr  (b: Double): Double = rd.difSqr   (d, b)
    def sumSqr  (b: Double): Double = rd.sumSqr   (d, b)
    def sqrSum  (b: Double): Double = rd.sqrSum   (d, b)
    def sqrDif  (b: Double): Double = rd.sqrDif   (d, b)
    def absDif  (b: Double): Double = rd.absDif   (d, b)
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

    def mod     (b: Double): Double = rd.mod      (d, b)

    // def firstarg( b: Double ) : Double  = d

    def linLin(inLow: Double, inHigh: Double, outLow: Double, outHigh: Double): Double =
      rd.linLin(d, inLow, inHigh, outLow, outHigh)

    def linExp(inLow: Double, inHigh: Double, outLow: Double, outHigh: Double): Double =
      rd.linExp(d, inLow, inHigh, outLow, outHigh)

    def expLin(inLow: Double, inHigh: Double, outLow: Double, outHigh: Double): Double =
      rd.expLin(d, inLow, inHigh, outLow, outHigh)

    def expExp(inLow: Double, inHigh: Double, outLow: Double, outHigh: Double): Double =
      rd.expExp(d, inLow, inHigh, outLow, outHigh)
  }

  //   sealed trait NAryDoubleOps2 extends NAryDoubleOps {
  //      def round( b: Double ) : Double     = rd.round( d, b )
  //      def roundup( b: Double ) : Double   = rd.roundup( d, b )
  //      def trunc( b: Double ) : Double     = rd.trunc( d, b )
  //   }
}
