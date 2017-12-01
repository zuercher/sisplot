package us.zuercher.sisplot

import com.twitter.util.{Return, Throw, Try}
import org.scalatest.{FunSpec, Matchers}

class ContextTest extends FunSpec with Matchers
{
  describe("RuntimeContext") {
    it("should error on undefined vars") {
      val ctxt = new RuntimeContext(NoopRenderTarget)
      ctxt.valueOf("x").isThrow should be(true)
    }

    it("should assign and return vars") {
      val ctxt = new RuntimeContext(NoopRenderTarget)
      ctxt.assign("x", 1.0)
      ctxt.valueOf("x") should equal(Return(1.0))
    }

    it("should invoke functions") {
      val ctxt = new RuntimeContext(NoopRenderTarget)
      ctxt.dispatch("cos", Seq(0.0)) should equal(Return(1.0))
    }

    it("should invoke the renderTarget on render") {
      var invoked = false
      val ctxt = new RuntimeContext(new RenderTarget {
        def apply(cmd: RenderCmd): Try[Unit] = {
          Try {
            cmd match {
              case Vertex(r, theta) =>
                r should equal(1.0)
                theta should equal(2.0)
                invoked = true
                ()

              case _ => throw new Exception("expected Vertex")
            }
          }
        }
        def close() = Return.Unit
      })

      ctxt.dispatch("render", Seq(1.0, 2.0)) should equal(Return(0.0))

      invoked should be(true)
    }

    describe("range") {
      describe("exclusive") {
        it("should use a default step") {
          val ctxt = new RuntimeContext(NoopRenderTarget)

          ctxt.range(0.0, 0.05, None, false).toList should equal(List(0.0, 0.01, 0.02, 0.03, 0.04))
        }

        it("should use a custom step") {
          val ctxt = new RuntimeContext(NoopRenderTarget)

          ctxt.range(0.0, 0.05, Some(0.02), false).toList should equal(List(0.0, 0.02, 0.04))
        }

        it("should use a default negative step") {
          val ctxt = new RuntimeContext(NoopRenderTarget)

          ctxt.range(0.0, -0.05, None, false).toList should equal(
            List(0.0, -0.01, -0.02, -0.03, -0.04)
          )
        }

        it("should use a custom negative step") {
          val ctxt = new RuntimeContext(NoopRenderTarget)

          ctxt.range(5, 0, Some(-1), false).toList should equal(List(5.0, 4.0, 3.0, 2.0, 1.0))
        }
      }

      describe("inclusive") {
        it("should use a default step") {
          val ctxt = new RuntimeContext(NoopRenderTarget)

          ctxt.range(0.0, 0.05, None, true).toList should equal(
            List(0.0, 0.01, 0.02, 0.03, 0.04, 0.05)
          )
        }

        it("should use a custom step") {
          val ctxt = new RuntimeContext(NoopRenderTarget)

          ctxt.range(0.0, 0.05, Some(0.02), true).toList should equal(List(0.0, 0.02, 0.04))
        }

        it("should use a default negative step") {
          val ctxt = new RuntimeContext(NoopRenderTarget)

          ctxt.range(0.0, -0.05, None, true).toList should equal(
            List(0.0, -0.01, -0.02, -0.03, -0.04, -0.05)
          )
        }

        it("should use a custom negative step") {
          val ctxt = new RuntimeContext(NoopRenderTarget)

          ctxt.range(5, 0, Some(-1), true).toList should equal(List(5.0, 4.0, 3.0, 2.0, 1.0, 0.0))
        }
      }
    }
  }
}
