/*
 *  IntFunctions2.scala
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

package de.sciss.numbers

/** Less common functions for `Int` values. */
object IntFunctions2 {
  @inline def cubed   (a: Int): Long  = { val n = a.toLong; n * n * n }
}
