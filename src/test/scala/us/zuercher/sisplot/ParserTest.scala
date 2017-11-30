package us.zuercher.sisplot

import com.twitter.util.{Return, Throw, Try}
import org.scalatest.{FunSpec, Matchers}
import scala.math

object ExprParser
{
  def apply(input: String): Try[Expr] = {
    Parser("x = %s".format(input)).flatMap {
      case List(Assignment(_, expr)) => Return(expr)
      case x => Throw(new Exception("unexpected Return: " + x.toString))
    }
  }
}

class ParserTest extends FunSpec with Matchers
{
  describe("Parser") {
    describe("basic expressions") {
      it("should parse floating point numbers") {
        ExprParser("1.0") should equal(Return(Constant(1.0)))
        ExprParser("2") should equal(Return(Constant(2.0)))
        ExprParser(".5") should equal(Return(Constant(0.5)))
        ExprParser("+1.0") should equal(Return(Constant(1.0)))
        ExprParser("+2") should equal(Return(Constant(2.0)))
        ExprParser("+.5") should equal(Return(Constant(0.5)))
        ExprParser("-1.0") should equal(Return(Constant(-1.0)))
        ExprParser("-2") should equal(Return(Constant(-2.0)))
        ExprParser("-.5") should equal(Return(Constant(-0.5)))
      }

      it("should evaluate numeric expressions") {
        ExprParser("1.0+0.5") should equal(Return(Add(Constant(1.0), Constant(0.5))))
        ExprParser("1.0+-0.5") should equal(Return(Add(Constant(1.0), Constant(-0.5))))
        ExprParser("-1.0+0.5") should equal(Return(Add(Constant(-1.0), Constant(0.5))))
        ExprParser("-1.0+-0.5") should equal(Return(Add(Constant(-1.0), Constant(-0.5))))

        ExprParser("1.0-0.5") should equal(Return(Subtract(Constant(1.0), Constant(0.5))))
        ExprParser("1.0--0.5") should equal(Return(Subtract(Constant(1.0), Constant(-0.5))))
        ExprParser("-1.0-0.5") should equal(Return(Subtract(Constant(-1.0), Constant(0.5))))
        ExprParser("-1.0--0.5") should equal(Return(Subtract(Constant(-1.0), Constant(-0.5))))

        ExprParser("2*4") should equal(Return(Multiply(Constant(2.0), Constant(4.0))))
        ExprParser("-2*4") should equal(Return(Multiply(Constant(-2.0), Constant(4.0))))
        ExprParser("2*-4") should equal(Return(Multiply(Constant(2.0), Constant(-4.0))))
        ExprParser("-2*-4") should equal(Return(Multiply(Constant(-2.0), Constant(-4.0))))

        ExprParser("2/4") should equal(Return(Divide(Constant(2.0), Constant(4.0))))
        ExprParser("-2/4") should equal(Return(Divide(Constant(-2.0), Constant(4.0))))
        ExprParser("2/-4") should equal(Return(Divide(Constant(2.0), Constant(-4.0))))
        ExprParser("-2/-4") should equal(Return(Divide(Constant(-2.0), Constant(-4.0))))
      }

      it("should evaluate numeric expressions with operator precedence") {
        ExprParser("1 + 2 * 3") should equal(
          Return(Add(Constant(1.0), Multiply(Constant(2.0), Constant(3.0))))
        )

        ExprParser("1 + 2 / 3") should equal(
          Return(Add(Constant(1.0), Divide(Constant(2.0), Constant(3.0))))
        )
      }

      it("should evaluate numeric expressions with grouping") {
        ExprParser("(1 + 2) * 3") should equal(
          Return(Multiply(Add(Constant(1.0), Constant(2.0)), Constant(3.0)))
        )

        ExprParser("(1 + 2) / 3") should equal(
          Return(Divide(Add(Constant(1.0), Constant(2.0)), Constant(3.0)))
        )
      }

      it("should parse pi") {
        ExprParser("pi") should equal(Return(Constant(math.Pi)))
        ExprParser("π") should equal(Return(Constant(math.Pi)))
      }

      it("should parse e") {
        ExprParser("e") should equal(Return(Constant(math.E)))
      }

      it("should parse variables") {
        ExprParser("(3*pi + x) / 2") should equal(
          Return(
            Divide(
              Add(
                Multiply(Constant(3.0), Constant(math.Pi)),
                Variable("x")
              ),
              Constant(2.0)
            )
          )
        )
      }

      it("should parse function calls") {
        ExprParser("ex(1)") should equal(Return(Call("ex", List(Constant(1.0)))))
        ExprParser("pick(1, 2)") should equal(
          Return(Call("pick", List(Constant(1.0), Constant(2.0))))
        )
        ExprParser("xypdq(1+2)") should equal(
          Return(Call("xypdq", List(Add(Constant(1.0), Constant(2.0)))))
        )
      }

      it("should parse nested function calls") {
        ExprParser("x(y(z(1)))") should equal(
          Return(
            Call(
              "x",
              List(
                Call(
                  "y",
                  List(
                    Call("z", List(Constant(1.0)))
                  )
                )
              )
            )
          )
        )
      }

      it("should return errors") {
        ExprParser("1 +").isThrow should be(true)
        ExprParser("+ 1").isThrow should be(true)
        ExprParser("1 -").isThrow should be(true)
        ExprParser("- 1").isThrow should be(true)
        ExprParser("1 *").isThrow should be(true)
        ExprParser("* 1").isThrow should be(true)
        ExprParser("1 /").isThrow should be(true)
        ExprParser("/ 1").isThrow should be(true)
        ExprParser("(1 + 2").isThrow should be(true)
      }
    }

    describe("assignments") {
      it("should assign a variable") {
        Parser("x = 1") should equal(Return(List(Assignment("x", Constant(1.0)))))
      }

      it("should assign repeatedly") {
        Parser("x = 1 y = 2 + 3") should equal(
          Return(List(
            Assignment("x", Constant(1.0)),
            Assignment("y", Add(Constant(2.0), Constant(3.0))),
          ))
        )
      }

      it("should not assign pi") {
        Parser("pi = 1") should equal(
          Return(List(StatementError("3.14159 is not a variable")))
        )

        Parser("π = 1") should equal(
          Return(List(StatementError("3.14159 is not a variable")))
        )
      }

      it("should not assign e") {
        Parser("e = 1") should equal(
          Return(List(StatementError("2.71828 is not a variable")))
        )
      }
    }

    describe("void calls") {
      it("should invoke a function") {
        Parser("render(0, 0)") should equal(
          Return(List(VoidCall(Call("render", Seq(Constant(0.0), Constant(0.0))))))
        )
      }
    }

    describe("loops") {
      it("should assign in a loop") {
        Parser("""
          for x over [0, 100) {
            y = x + 5
            z = y / 10
          }"""
        ) should equal(
          Return(List(
            Loop(
              "x",
              Constant(0),
              Constant(100),
              None,
              false,
              List(
                Assignment("y", Add(Variable("x"), Constant(5))),
                Assignment("z", Divide(Variable("y"), Constant(10))),
              )
            )
          ))
        )
      }

      it("should assign in a loop with inclusive end boundary") {
        Parser("""
          for x over [0, 100] {
            y = x + 5
          }"""
        ) should equal(
          Return(List(
            Loop(
              "x",
              Constant(0),
              Constant(100),
              None,
              true,
              List(Assignment("y", Add(Variable("x"), Constant(5))))
            )
          ))
        )
      }

      it("should allow step in a loop") {
        Parser("""
          for x over [0, 100) by 10 {
            y = x + 5
            z = y / 10
          }"""
        ) should equal(
          Return(List(
            Loop(
              "x",
              Constant(0),
              Constant(100),
              Some(Constant(10)),
              false,
              List(
                Assignment("y", Add(Variable("x"), Constant(5))),
                Assignment("z", Divide(Variable("y"), Constant(10))),
              )
            )
          ))
        )
      }

      it("should allow negative step in a loop") {
        Parser("""
          for x over [100, 0) by -10 {
            y = x + 5
            z = y / 10
          }"""
        ) should equal(
          Return(List(
            Loop(
              "x",
              Constant(100),
              Constant(0),
              Some(Constant(-10)),
              false,
              List(
                Assignment("y", Add(Variable("x"), Constant(5))),
                Assignment("z", Divide(Variable("y"), Constant(10))),
              )
            )
          ))
        )
      }

      it("should nest loops") {
        Parser("""
          for x over [0, 1) {
            for y over [2, 3) {
              z = x * y
            }
          }"""
        ) should equal(
          Return(List(
            Loop(
              "x",
              Constant(0),
              Constant(1),
              None,
              false,
              List(
                Loop(
                  "y",
                  Constant(2),
                  Constant(3),
                  None,
                  false,
                  List(
                    Assignment("z", Multiply(Variable("x"), Variable("y")))
                  )
                )
              )
            )
          ))
        )
      }

      it("should take variables as loop parameters") {
        Parser("""
          for x over [0, 1) {
            q = x * 10
            for y over [q, q + 1)  {
              z = x * y
            }
          }"""
        ) should equal(
          Return(List(
            Loop(
              "x",
              Constant(0),
              Constant(1),
              None,
              false,
              List(
                Assignment("q", Multiply(Variable("x"), Constant(10))),
                Loop(
                  "y",
                  Variable("q"),
                  Add(Variable("q"), Constant(1)),
                  None,
                  false,
                  List(
                    Assignment("z", Multiply(Variable("x"), Variable("y")))
                  )
                )
              )
            )
          ))
        )
      }
    }

    describe("validation") {
      it("should detect undefined variables") {
        val t = Parser("q = 1\nz = x").flatMap(Parser.validate)
        t.isThrow should be(true)
        t.throwable.getMessage should equal("assignment error at line 2, column 1")
        t.throwable.getCause should not be(null)
        t.throwable.getCause.getMessage should equal("undefined variable x")
      }

      it("should detect illegal assignments") {
        val t = Parser("""pi = 1""").flatMap(Parser.validate)
        t.isThrow should be(true)
        t.throwable.getMessage should equal("3.14159 is not a variable at line 1, column 1")
      }

      it("should detect invalid loop ranges") {
        val t = Parser("for x over [1, 0) by 1 { z = x }").flatMap(Parser.validate)
        t.isThrow should be(true)
        t.throwable.getMessage should equal("loop error at line 1, column 1")
        t.throwable.getCause should not be(null)
        t.throwable.getCause.getMessage should equal(
          "must use negative step for start (1.00000) > end (0.00000)"
        )

        val t2 = Parser("for x over [0, 1) by -1 { z = x }").flatMap(Parser.validate)
        t2.isThrow should be(true)
        t2.throwable.getMessage should equal("loop error at line 1, column 1")
        t2.throwable.getCause should not be(null)
        t2.throwable.getCause.getMessage should equal(
          "must use positive step for start (0.00000) < end (1.00000)"
        )

        val t3 = Parser("for x over [0, 1) by 0 { z = x }").flatMap(Parser.validate)
        t3.isThrow should be(true)
        t3.throwable.getMessage should equal("loop error at line 1, column 1")
        t3.throwable.getCause should not be(null)
        t3.throwable.getCause.getMessage should equal(
          "cannot use zero step unless start (0.00000) == end (1.00000)"
        )
      }

      it("should fail on invalid function names") {
        val t = Parser("_ = snoopy(1.0)").flatMap(Parser.validate)
        t.isThrow should be(true)
        t.throwable.getMessage should equal("assignment error at line 1, column 1")
        t.throwable.getCause should not be(null)
        t.throwable.getCause.getMessage should equal(
          "snoopy: no such function"
        )
      }

      it("should allow no-op assignments") {
        Parser("_ = 1").flatMap(Parser.validate) should equal(
          Return(List(Assignment("_", Constant(1.0))))
        )
      }

      it("should allow reading from the _ variable") {
        val t = Parser("_ = 1\nx = _").flatMap(Parser.validate)
        t.isThrow should be(true)
      }

      it("should fail on too many function arguments") {
        val t = Parser("_ = cos(1.0, 2.0)").flatMap(Parser.validate)
        t.isThrow should be(true)
        t.throwable.getMessage should equal("assignment error at line 1, column 1")
        t.throwable.getCause should not be(null)
        t.throwable.getCause.getMessage should equal(
          "wrong number of arguments: requires 1, got 2"
        )
      }

      it("should fail on too few function arguments") {
        val t = Parser("_ = pow(1.0)").flatMap(Parser.validate)
        t.isThrow should be(true)
        t.throwable.getMessage should equal("assignment error at line 1, column 1")
        t.throwable.getCause should not be(null)
        t.throwable.getCause.getMessage should equal(
          "wrong number of arguments: requires 2, got 1"
        )
      }
    }
  }
}
