package us.zuercher.sisplot

import com.twitter.util.{Return, Throw, Try}
import org.scalatest.{FunSpec, Matchers}

class StatementTest extends FunSpec with Matchers
{
  describe("Statement") {
    describe("Assignment") {
      it("should set variables in the context") {
        val ctxt = new RuntimeContext((_, _) => Return.Unit)

        Assignment("thevar", Constant(1.0)).execute(ctxt) should equal(Return.Unit)

        ctxt.valueOf("thevar") should equal(Return(1.0))
      }
    }

    describe("VoidCall") {
      it("should call and ignore the result") {
        var invoked = false
        val ctxt = new RuntimeContext((_, _) => {
          invoked = true
          Return.Unit
        })

        VoidCall(Call("render", Seq(Constant(0), Constant(0)))).execute(ctxt) should equal(
          Return.Unit
        )

        invoked should be(true)
      }
    }

    describe("Loop") {
      it("should validate range and step") {
        val ctxt = new RuntimeContext((_, _) => Return.Unit)

        Loop("x", Constant(1), Constant(0), Some(Constant(0.5)), false, Seq.empty)
          .execute(ctxt).isThrow should be(true)
        Loop("x", Constant(0), Constant(1), Some(Constant(-0.5)), false, Seq.empty)
          .execute(ctxt).isThrow should be(true)
        Loop("x", Constant(0), Constant(1), Some(Constant(0)), false, Seq.empty)
          .execute(ctxt).isThrow should be(true)
      }

      it("should iterate, assigning the variable and invoking statements with a default step") {
        var values = List[Double]()

        val ctxt = new RuntimeContext((r, theta) => {
          values = values :+ r
          theta should equal(0.0)
          Return.Unit
        })

        Loop(
          "x",
          Constant(0),
          Constant(0.05),
          None,
          false,
          Seq(Assignment("_", Call("render", Seq(Variable("x"), Constant(0.0)))))
        ).execute(ctxt) should equal(Return.Unit)

        values should equal(List(0.0, 0.01, 0.02, 0.03, 0.04))
      }

      it("should iterate, assigning the variable and invoking statements with a custom step") {
        var values = List[Double]()

        val ctxt = new RuntimeContext((r, theta) => {
          values = values :+ r
          theta should equal(0.0)
          Return.Unit
        })

        Loop(
          "x",
          Constant(0),
          Constant(5),
          Some(Constant(1.0)),
          false,
          Seq(Assignment("_", Call("render", Seq(Variable("x"), Constant(0.0)))))
        ).execute(ctxt) should equal(Return.Unit)

        values should equal(List(0.0, 1.0, 2.0, 3.0, 4.0))
      }
    }
  }
}
