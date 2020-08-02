/*
 *  RichDouble.scala
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
  def midiCps   : Double = rd.midiCps   (d)
  def cpsMidi   : Double = rd.cpsMidi   (d)
  def midiRatio : Double = rd.midiRatio (d)
  def ratioMidi : Double = rd.ratioMidi (d)
  def dbAmp     : Double = rd.dbAmp     (d)
  def ampDb     : Double = rd.ampDb     (d)
  def octCps    : Double = rd.octCps    (d)
  def cpsOct    : Double = rd.cpsOct    (d)
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
  //   def softClip : Double   = rd.softClip( d )
  //   def ramp : Double       = rd.ramp( d )
  //   def sCurve : Double     = rd.sCurve( d )

  def rectWindow : Double = rd2.rectWindow  (d)
  def hannWindow : Double = rd2.hannWindow  (d)
  def welchWindow: Double = rd2.welchWindow (d)
  def triWindow  : Double = rd2.triWindow   (d)
}