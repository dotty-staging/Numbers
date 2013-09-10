package de.sciss.numbers

import org.scalatest.FunSpec

/** To run only this test:
  *
  * test-only de.sciss.numbers.NumbersSuite
  */
class NumbersSuite extends FunSpec {
  import Implicits._

  describe("Rich number operators") {
    it("should properly expand to primitives and GE") {
      val intInt1 = 6.roundTo(4)
      assert(intInt1 === 8f)
      assert(intInt1.isInstanceOf[Float], "found " + intInt1.getClass)
    }
  }
}