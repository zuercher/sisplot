package us.zuercher.sisplot

import com.twitter.util.{Return, Throw, Try}
import scala.util.parsing.input.{Position, Positional}

private object Statement
{
  val ReturnUnit: Try[Unit] = Return.Unit
}

/*
 * Statement represents a statement in the sisplot language. All
 * Statements have a position assigned by the Parser.
 */
sealed trait Statement extends Positional
{
  def execute(ctxt: Context): Try[Unit]
}

/*
 * StatementError indicates an invalid statement.
 */
case class StatementError(message: String) extends Statement
{
  override def execute(ctxt: Context): Try[Unit] = {
    Parser.errorWithPos(message, pos)
  }
}

/*
 * Assignment represents a variable assignment statement.
 */
case class Assignment(variable: String, expr: Expr) extends Statement
{
  override def execute(ctxt: Context): Try[Unit] = {
    expr.evaluate(ctxt).map { value => ctxt.assign(variable, value) } match {
      case r@Return(_) => r
      case t@Throw(_)  => Parser.errorWithPos("assignment error", pos, Some(t))
    }
  }
}

/*
 * VoidCall represents a function call whose result is not assigned.
 */
case class VoidCall(call: Call) extends Statement
{
  override def execute(ctxt: Context): Try[Unit] = {
    call.evaluate(ctxt) match {
      case Return(_)  => Return.Unit
      case t@Throw(_) => Parser.errorWithPos("call error", pos, Some(t))
    }
  }
}

/*
 * Loop represents a for loop with a variable, start and end values
 * and a block of Statements. An optional step may be provided. If
 * inclusive is true, the end value is included in the iteration
 * (provided the step allows this).
 */
case class Loop(
  variable: String,
  start: Expr,
  end: Expr,
  step: Option[Expr],
  inclusive: Boolean,
  statements: Seq[Statement]
) extends Statement
{
  override def execute(ctxt: Context): Try[Unit] = {
    val rangeParams = start.evaluate(ctxt).flatMap { s =>
      end.evaluate(ctxt).flatMap { e =>
        step match {
          case Some(stepExpr) => stepExpr.evaluate(ctxt).map { v => (s, e, Some(v)) }
          case None           => Return(s, e, None)
        }
      }
    }.flatMap {
      case (start, end, Some(step)) if start != end && step == 0 =>
        Throw(
          new Exception("cannot use zero step unless start (%g) == end (%g)".format(start, end))
        )
      case (start, end, Some(step)) if start > end && step > 0 =>
        Throw(
          new Exception("must use negative step for start (%g) > end (%g)".format(start, end))
        )
      case (start, end, Some(step)) if start < end && step < 0 =>
        Throw(
          new Exception("must use positive step for start (%g) < end (%g)".format(start, end))
        )
      case ok => Return(ok)
    }

    rangeParams.flatMap { case (start, end, step) =>
      ctxt.range(start, end, step, inclusive).foldLeft(Statement.ReturnUnit) { (t, value) =>
        ctxt.assign(variable, value)
        t.flatMap { _ =>
          statements.foldLeft(Statement.ReturnUnit) { (t2, stmt) =>
            t2.flatMap { _ => stmt.execute(ctxt) }
          }
        }
      }
    } match {
      case r@Return(_) => r
      case t@Throw(_)  => Parser.errorWithPos("loop error", pos, Some(t))
    }
  }
}
