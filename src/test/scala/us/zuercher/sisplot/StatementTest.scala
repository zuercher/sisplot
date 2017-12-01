package us.zuercher.sisplot

import com.twitter.util.{Return, Throw, Try}
import org.scalatest.{FunSpec, Matchers}

class StatementTest extends FunSpec with Matchers
{
  describe("Statement") {
    describe("Assignment") {
      it("should set variables in the context") {
        val ctxt = new RuntimeContext(NoopRenderTarget)

        Assignment("thevar", Constant(1.0)).execute(ctxt) should equal(Return.Unit)

        ctxt.valueOf("thevar") should equal(Return(1.0))
      }
    }

    describe("VoidCall") {
      it("should call and ignore the result") {
        val counter = new CountingRenderTarget
        val ctxt = new RuntimeContext(counter)

        VoidCall(Call("render", Seq(Constant(0), Constant(0)))).execute(ctxt) should equal(
          Return.Unit
        )

        counter.count() should equal(1)
      }
    }

    describe("Loop") {
      it("should validate range and step") {
        val ctxt = new RuntimeContext(NoopRenderTarget)

        Loop("x", Constant(1), Constant(0), Some(Constant(0.5)), false, Seq.empty)
          .execute(ctxt).isThrow should be(true)
        Loop("x", Constant(0), Constant(1), Some(Constant(-0.5)), false, Seq.empty)
          .execute(ctxt).isThrow should be(true)
        Loop("x", Constant(0), Constant(1), Some(Constant(0)), false, Seq.empty)
          .execute(ctxt).isThrow should be(true)
      }

      it("should iterate, assigning the variable and invoking statements with a default step") {
        val buffer = new BufferingRenderTarget(NoopRenderTarget)
        val ctxt = new RuntimeContext(buffer)

        Loop(
          "x",
          Constant(0),
          Constant(0.05),
          None,
          false,
          Seq(Assignment("_", Call("render", Seq(Variable("x"), Constant(0.0)))))
        ).execute(ctxt) should equal(Return.Unit)

        buffer.buffer.collect {
          case Vertex(r, _) => r
        }  should equal(List(0.0, 0.01, 0.02, 0.03, 0.04))
      }

      it("should iterate, assigning the variable and invoking statements with a custom step") {
        val buffer = new BufferingRenderTarget(NoopRenderTarget)
        val ctxt = new RuntimeContext(buffer)

        Loop(
          "x",
          Constant(0),
          Constant(5),
          Some(Constant(1.0)),
          false,
          Seq(Assignment("_", Call("render", Seq(Variable("x"), Constant(0.0)))))
        ).execute(ctxt) should equal(Return.Unit)

        buffer.buffer.collect {
          case Vertex(r, _) => r
        }  should equal(List(0.0, 1.0, 2.0, 3.0, 4.0))
      }
    }
  }
}
