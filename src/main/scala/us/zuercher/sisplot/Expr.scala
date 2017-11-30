package us.zuercher.sisplot

import com.twitter.util.{Return, Throw, Try}
import scala.util.parsing.input.Positional

/*
 * Expr represents an expression in the sisbot language.
 */
sealed trait Expr extends Positional
{
  def evaluate(ctxt: Context): Try[Double]

  def description: String
}

/*
 * Variable represents a Variable lookup.
 */
case class Variable(name: String) extends Expr
{
  override def evaluate(ctxt: Context): Try[Double] = { ctxt.valueOf(name) }

  override val description = "%s".format(name)
}

/*
 * Constant represents a constant value.
 */
case class Constant(value: Double) extends Expr
{
  override def evaluate(ctxt: Context): Try[Double] = { Return(value) }

  override val description = "%g".format(value)
}

/*
 * Add represents the addition of two expressions.
 */
case class Add(a: Expr, b: Expr) extends Expr
{
  override def evaluate(ctxt: Context): Try[Double] = {
    a.evaluate(ctxt).flatMap { av => b.evaluate(ctxt).map { bv => av + bv } }
  }

  override def description = "(%s) + (%s)".format(a.description, b.description)
}

/*
 * Subtract represents the subtration of two expressions.
 */
case class Subtract(a: Expr, b: Expr) extends Expr
{
  override def evaluate(ctxt: Context): Try[Double] = {
    a.evaluate(ctxt).flatMap { av => b.evaluate(ctxt).map { bv => av - bv } }
  }

  override def description = "(%s) - (%s)".format(a.description, b.description)
}

/*
 * Multiply represents the multiplication of two expressions.
 */
case class Multiply(a: Expr, b: Expr) extends Expr
{
  override def evaluate(ctxt: Context): Try[Double] = {
    a.evaluate(ctxt).flatMap { av => b.evaluate(ctxt).map { bv => av * bv } }
  }

  override def description = "(%s) * (%s)".format(a.description, b.description)
}

/*
 * Divide represents the division of two expressions.
 */
case class Divide(a: Expr, b: Expr) extends Expr
{
  override def evaluate(ctxt: Context): Try[Double] = {
    a.evaluate(ctxt).flatMap { av => b.evaluate(ctxt).map { bv => av / bv } }
  }

  override def description = "(%s) / (%s)".format(a.description, b.description)
}

/*
 * Call represents a function call with zero or more arguments.
 */
case class Call(name: String, args: Seq[Expr]) extends Expr
{
  override def evaluate(ctxt: Context): Try[Double] = {
    val argValues = Try.collect(args.map { _.evaluate(ctxt) })
    argValues match {
      case Return(values) => ctxt.dispatch(name, values)
      case Throw(t)       => Parser.errorWithPos("call argument error", pos)
    }
  }

  override def description = "%s(%s)".format(name, args.map { _.description }.mkString(", "))
}
