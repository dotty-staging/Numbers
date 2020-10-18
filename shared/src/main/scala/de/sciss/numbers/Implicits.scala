/*
 *  Implicits.scala
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

import language.implicitConversions

object Implicits {
  implicit def intNumberWrapper   (i: Int   ): RichInt    = new RichInt   (i)
  implicit def floatNumberWrapper (f: Float ): RichFloat  = new RichFloat (f)
  implicit def doubleNumberWrapper(d: Double): RichDouble = new RichDouble(d)
}
