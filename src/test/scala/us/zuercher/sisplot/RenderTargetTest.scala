package us.zuercher.sisplot

import com.twitter.util.Return
import java.io.{StringWriter, Writer}
import org.scalatest.{FunSpec, Matchers}

class FailingWriter extends Writer {
  override def write(char: Array[Char], offset: Int, length: Int) {
    throw new Exception("boom")
  }
  override def close() {}
  override def flush() {}
}

class RenderTargetTest extends FunSpec with Matchers
{
  describe("VertsRenderTarget") {
    it("should write vertices to the given writer") {
      val sw = new StringWriter
      val rt = new VertsRenderTarget(sw)
      rt.vertex(1.0, 2.0) should equal(Return.Unit)
      rt.vertex(2.0, 3.0) should equal(Return.Unit)
      rt.vertex(3.0, 4.0) should equal(Return.Unit)
      rt.close() should equal(Return.Unit)

      sw.toString should equal(
        List((2.0, 1.0), (3.0, 2.0), (4.0, 3.0))
          .map { case (r, t) => "%.5f %.5f\n".format(r, t) }.mkString("")
      )
    }

    it("should throw on error") {
      val rt = new VertsRenderTarget(new FailingWriter)

      rt.vertex(1.0, 2.0).isThrow should be(true)
    }
  }

  describe("BufferingRenderTarget") {
    it("should buffer vertices until close") {
      val sw = new StringWriter
      val rt = new BufferingRenderTarget(new VertsRenderTarget(sw))
      rt.vertex(1.0, 2.0) should equal(Return.Unit)
      rt.vertex(2.0, 3.0) should equal(Return.Unit)
      rt.vertex(3.0, 4.0) should equal(Return.Unit)

      sw.toString should be('empty)
      rt.close() should equal(Return.Unit)

      sw.toString should equal(
        List((2.0, 1.0), (3.0, 2.0), (4.0, 3.0))
          .map { case (r, t) => "%.5f %.5f\n".format(r, t) }.mkString("")
      )
    }

    it("should throw errors on close") {
      val rt = new BufferingRenderTarget(new VertsRenderTarget(new FailingWriter))

      rt.vertex(1.0, 2.0) should equal(Return.Unit)
      rt.close().isThrow should be(true)
    }
  }

  describe("NormalizingRenderTarget") {
    it("should normalize large radii to |1.0|") {
      val sw = new StringWriter
      val rt = new NormalizingRenderTarget(new VertsRenderTarget(sw))
      rt.vertex(1.0, 1.0) should equal(Return.Unit)
      rt.vertex(4.0, 2.0) should equal(Return.Unit)
      rt.vertex(-2.0, 3.0) should equal(Return.Unit)
      rt.close() should equal(Return.Unit)

      sw.toString should equal(
        List((1.0, 0.25), (2.0, 1.0), (3.0, -0.5))
          .map { case (r, t) => "%.5f %.5f\n".format(r, t) }.mkString("")
      )
    }

    it("should normalize small radii to |1.0|") {
      val sw = new StringWriter
      val rt = new NormalizingRenderTarget(new VertsRenderTarget(sw))
      rt.vertex(0.25, 1.0) should equal(Return.Unit)
      rt.vertex(0.5, 2.0) should equal(Return.Unit)
      rt.vertex(-0.125, 3.0) should equal(Return.Unit)
      rt.close() should equal(Return.Unit)

      sw.toString should equal(
        List((1.0, 0.5), (2.0, 1.0), (3.0, -0.25))
          .map { case (r, t) => "%.5f %.5f\n".format(r, t) }.mkString("")
      )
    }

    it("should not normalize theta to 2π") {
      val sw = new StringWriter
      val rt = new NormalizingRenderTarget(new VertsRenderTarget(sw))

      val pi = math.Pi
      val twoPi = math.Pi*2.0
      val threePi = math.Pi*3.0
      rt.vertex(1.0, pi) should equal(Return.Unit)
      rt.vertex(1.0, twoPi) should equal(Return.Unit)
      rt.vertex(1.0, threePi) should equal(Return.Unit)
      rt.vertex(1.0, -threePi) should equal(Return.Unit)
      rt.vertex(1.0, -twoPi) should equal(Return.Unit)
      rt.close() should equal(Return.Unit)

      sw.toString should equal(
        List((pi, 1.0), (twoPi, 1.0), (threePi, 1.0), (-threePi, 1.0), (-twoPi, 1.0))
          .map { case (r, t) => "%.5f %.5f\n".format(r, t) }.mkString("")
      )
    }
  }
}
