package us.zuercher.sisplot

import com.twitter.util.{Return, Throw, Try}
import org.scalatest.{FunSpec, Matchers}
import scala.math

object ExprTestContext extends RuntimeContext(FailingRenderTarget)
{
  assign("a", 1.0)
  assign("b", 2.0)
  assign("c", 0.0)
}

class ExprTest extends FunSpec with Matchers
{
  describe("Expr") {
    describe("Variable") {
      it("should lookup existing variables") {
        Variable("a").evaluate(ExprTestContext) should equal(Return(1.0))
      }

      it("should fail on non-existent variables") {
        Variable("nope").evaluate(ExprTestContext).isThrow should be(true)
      }
    }

    describe("Constant") {
      it("should return the constant value") {
        Constant(1.1).evaluate(ExprTestContext) should equal(Return(1.1))
      }
    }

    describe("Add") {
      it("should add the constituent expressions") {
        Add(Constant(1.0), Constant(2.0)).evaluate(ExprTestContext) should equal(Return(3.0))
      }
    }

    describe("Subtract") {
      it("should subtract the constituent expressions") {
        Subtract(Constant(1.0), Constant(2.0)).evaluate(ExprTestContext) should equal(Return(-1.0))
      }
    }

    describe("Multiply") {
      it("should multiply the constituent expressions") {
        Multiply(Constant(2.0), Constant(3.0)).evaluate(ExprTestContext) should equal(Return(6.0))
      }
    }

    describe("Divide") {
      it("should divide the constituent expressions") {
        Divide(Constant(2.0), Constant(3.0)).evaluate(ExprTestContext) should equal(Return(2.0/3.0))
      }
    }

    describe("Call") {
      it("should invoke the given function") {
        Call("cos", Seq(Constant(0.0))).evaluate(ExprTestContext) should equal(Return(1.0))
        Call("cos", Seq(Constant(math.Pi / 2.0))).evaluate(ExprTestContext) should equal(
          Return(math.cos(math.Pi / 2.0))
        )
      }

      it("should return a throw for the wrong number of arguments") {
        Call("sin", Seq(Constant(0.0), Constant(0.0))).evaluate(ExprTestContext).isThrow should be(
          true
        )
        Call("pow", Seq(Constant(0.0))).evaluate(ExprTestContext).isThrow should be(true)
      }

      it("should successfully call all functions") {
        val counter = new CountingRenderTarget
        val ctxt = new RuntimeContext(counter)

        Functions.AllNames.foreach { name =>
          val numArgs = Functions.NumArgsFor(name).get

          val args = Seq.fill(numArgs)(Constant(1.0))

          // Don't care about the value we get, but shouldn't see any Throws.
          Call(name, args).evaluate(ctxt).map { _ => Unit } should equal(Return(Unit))
        }

        // At least one render call
        counter.count() should be >= 1
      }
    }
  }
}
