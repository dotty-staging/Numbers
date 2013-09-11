package de.sciss.numbers

import org.scalatest.FunSpec
import collection.immutable.{IndexedSeq => Vec}

/** To run only this test:
  *
  * test-only de.sciss.numbers.NumbersSuite
  */
class NumbersSuite extends FunSpec {
  import Implicits._

  describe("Rich int operators") {
    it("should do what the pope said") {
      val isquared = -4.squared
      assert(isquared === 16L)
      assert(isquared.isInstanceOf[Long], s"found ${isquared.getClass}")

      assert(! (-2).isPowerOfTwo)
      assert(! (-1).isPowerOfTwo)
      assert(    0 .isPowerOfTwo)
      assert(    1 .isPowerOfTwo)
      assert(    2 .isPowerOfTwo)
      assert(!   3 .isPowerOfTwo)
      assert(0x40000000.isPowerOfTwo)

      assert((-4).nextPowerOfTwo === 1)
      assert(  0 .nextPowerOfTwo === 1)
      assert(  1 .nextPowerOfTwo === 1)
      assert(  2 .nextPowerOfTwo === 2)
      assert(  3 .nextPowerOfTwo === 4)
      assert(0x40000000.nextPowerOfTwo === 0x40000000)

      intercept[IllegalArgumentException](0x40000001.nextPowerOfTwo)

      val iroundTo = 6.roundTo(4)
      assert(iroundTo === 8f)
      assert(iroundTo.isInstanceOf[Float], s"found ${iroundTo.getClass}")

      val irange  = -12 to 18
      val iclip   = irange map (_ clip (-3, 7))
      val ifold   = irange map (_ fold (-3, 7))
      val iwrap   = irange map (_ wrap (-3, 7))

      assert(iclip ===
        Vec(-3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7))

      assert(ifold ===
        Vec(6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -2))

      assert(iwrap ===
        Vec(-1, 0, 1, 2, 3, 4, 5, 6, 7, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7))
    }
  }

  // yes this is pretty crappy; reason being that we pasted some SuperCollider results which
  // seem to exhibit quite some floating point error against java math (octcps notably)
  private final val epsf = 7.0e-5f

  private def assertFloat(a: Vec[Float], b: Vec[Float]) =
    assert((a zip b).forall { case (x, y) =>
      x.isNaN && y.isNaN || x == y ||
        { val z = (x absdif y) < epsf; if (!z) println(s"$x absdif $y = ${x absdif y}"); z }
    }, s"$a did not equal $b")

  describe("Rich float operators") {
    it("should do what the pope said") {
      val fseq1   = Vec(-4.0f, -4.3f, -0.0f, 4.0f, 4.3f)
      import Float.{NaN => nan, PositiveInfinity => inf}

      def assertMap(fun: Float => Float, b: Vec[Float]) =
        assertFloat(fseq1.map(fun), b)

      assertMap(_.frac      , Vec(0f, 0.7f, 0f, 0f, 0.3f))
      assertMap(_.squared   , Vec(16f, 18.49f, 0f, 16f, 18.49f))
      assertMap(_.sqrt      , Vec(nan, nan, 0f, 2f, 2.073644135332772f))
      assertMap(_.exp       , Vec(0.01831563888873418f, 0.013568556424203931f, 1f, 54.598150033144236f, 73.6998077567172f))
      assertMap(_.midicps   , Vec(6.4891358996866f, 6.3776564289715f, 8.1757989156437f, 10.300861153527f, 10.480917034883f))
      assertMap(_.cpsmidi   , Vec(nan, nan, -inf, -12.376316562296f, -11.124276644519f))
      assertMap(_.midiratio , Vec(0.79370052598483f, 0.78006522601415f, 1f, 1.2599210498937f, 1.2819440819195f))
      assertMap(_.ratiomidi , Vec(nan, nan, -inf, 24f, 25.252039917777f))
      assertMap(_.dbamp     , Vec(0.63095734448019f, 0.60953689724017f, 1f, 1.5848931924611f, 1.6405897731995f))
      assertMap(_.ampdb     , Vec(nan, nan, -inf, 12.041199826559f, 12.669369111592f))
      assertMap(_.octcps    , Vec(1.0219748644555f, 0.83010153266979f, 16.351597831287f, 261.6255653006f, 322.09885310804f))
      assertMap(_.cpsoct    , Vec(nan, nan, -inf, -2.031359713542f, -1.9270230537272f))

      // XXX TODO: log, log2, log10, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh
      // XXX TODO: binary ops
      // XXX TODO: n-ary ops (linlin, linexp, ...)

      val frange  = -12f to 18f by 1.5f
      val fclip   = frange map (_ clip (-3, 7))
      val ffold   = frange map (_ fold (-3, 7))
      val fwrap   = frange map (_ wrap (-3, 7))

      assert(fclip ===
        Vec(-3f, -3f, -3f, -3f, -3f, -3f, -3f, -1.5f, 0f, 1.5f, 3f, 4.5f, 6f, 7f, 7f, 7f, 7f, 7f, 7f, 7f, 7f))

      assert(ffold ===
        Vec(6f, 4.5f, 3f, 1.5f, 0f, -1.5f, -3f, -1.5f, 0f, 1.5f, 3f, 4.5f, 6f, 6.5f, 5f, 3.5f, 2f, 0.5f, -1f, -2.5f, -2f))

      assert(fwrap ===
        Vec(-2f, -0.5f, 1f, 2.5f, 4f, 5.5f, -3f, -1.5f, 0f, 1.5f, 3f, 4.5f, 6f, -2.5f, -1f, 0.5f, 2f, 3.5f, 5f, 6.5f, -2f))
    }
  }

  describe("Rich double operators") {
    it("should do what the pope said") {
      val drange  = -12.0 to 18.0 by 1.5
      val dclip   = drange map (_ clip (-3, 7))
      val dfold   = drange map (_ fold (-3, 7))
      val dwrap   = drange map (_ wrap (-3, 7))

      assert(dclip ===
        Vec(-3.0, -3.0, -3.0, -3.0, -3.0, -3.0, -3.0, -1.5, 0.0, 1.5, 3.0, 4.5, 6.0, 7.0, 7.0, 7.0, 7.0, 7.0, 7.0, 7.0, 7.0))

      assert(dfold ===
        Vec(6.0, 4.5, 3.0, 1.5, 0.0, -1.5, -3.0, -1.5, 0.0, 1.5, 3.0, 4.5, 6.0, 6.5, 5.0, 3.5, 2.0, 0.5, -1.0, -2.5, -2.0))

      assert(dwrap ===
        Vec(-2.0, -0.5, 1.0, 2.5, 4.0, 5.5, -3.0, -1.5, 0.0, 1.5, 3.0, 4.5, 6.0, -2.5, -1.0, 0.5, 2.0, 3.5, 5.0, 6.5, -2.0))
    }
  }
}
