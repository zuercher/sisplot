package us.zuercher.sisplot

import com.twitter.util.{Return, Throw, Try}
import java.io.{Reader, StringReader}
import scala.math
import scala.util.parsing.combinator._
import scala.util.parsing.input.Position

/*
 * Parser converts input into a Try[List[Statement]] of sisplot language statements.
 */
class Parser extends RegexParsers
{
  // A mathematical expression, constant, function call or variable lookup.
  def expr: Parser[Expr] = expr1 | constant | func | variable

  // Forces addition and subtraction to bind less tightly than
  // multiplication or division.
  def expr1: Parser[Expr] =
    positioned(
      expr2 ~ rep(addOrSubtract) ^^ {
        case a ~ b => (a /: b)((acc, f) => f(acc))
      }
    )

  def addOrSubtract: Parser[Expr => Expr] = """[+-]""".r ~ expr1 ^^ {
    case "+" ~ b => Add(_, b)
    case "-" ~ b => Subtract(_, b)
    case _ => throw new Exception("unreachable")
  }

  // Forces multiplication and division to bind less tightly than
  // a grouped expression.
  def expr2: Parser[Expr] =
    positioned(
      expr3 ~ rep(multiplyOrDivide) ^^ {
        case a ~ b => (a /: b)((acc, f) => f(acc))
      }
    )

  def multiplyOrDivide: Parser[Expr => Expr] = """[*/]""".r ~ expr3 ^^ {
    case "*" ~ b => Multiply(_, b)
    case "/" ~ b => Divide(_, b)
    case _ => throw new Exception("unreachable")
  }

  // A nested constant, function call, variable lookup or grouped expression.
  def expr3: Parser[Expr] = constant | func | variable | "(" ~> expr1 <~ ")"

  // A numerical constant. Allows optional leading +/-. Accepts
  // fractions with or without leading whole number (0.1 and .1 are
  // both valid). Accepts integers.
  def constant: Parser[Expr] =
    positioned(
      """[+-]?(?:\d*\.\d+|\d+)""".r ^^ { s =>
        Constant(java.lang.Double.valueOf(s))
      }
    )

  // A function argument list (sans parentheses) with at least one argument.
  def arglist: Parser[List[Expr]] = expr ~ opt(rep("," ~> expr)) ^^ {
    case first ~ None => List(first)
    case first ~ Some(rest) => first :: rest
  }

  // A function call. Function names are all-lowercase. They may
  // contain numbers and underscores after the first character.
  def func: Parser[Expr] =
    positioned(
      """[a-z][a-z0-9_]*""".r ~ ("(" ~> arglist <~ ")") ^^ {
        case name ~ args => Call(name, args)
      }
    )

  // A variable. Variable names start with a lower case letter
  // (English or Greek) or underscore. After the first character,
  // numeric digits are also allowed.
  def variable: Parser[Expr] =
    positioned(
      """[a-zα-ω_][a-z0-9α-ω_]*""".r ^^ {
        case "pi" => Constant(math.Pi)
        case "π" => Constant(math.Pi)
        case "e" => Constant(math.E)
        case s => Variable(s)
      }
    )

  // A list of statements: loops, assignments, and void function calls.
  def statements: Parser[List[Statement]] = rep1(loop | assignment | void_call)

  // An assignment of an expression to a variable name.
  def assignment: Parser[Statement] =
    positioned(
      variable ~ "=" ~ expr ^^ {
        case v@Variable(name) ~ "=" ~ e => Assignment(name, e)
        case x ~ "=" ~ _ => StatementError("%s is not a variable".format(x.description))
      }
    )

  // A void function call. (A function call whose value is not assigned to a variable.)
  def void_call: Parser[Statement] =
    positioned(
      func ^^ {
        case c@Call(_, _) => VoidCall(c)
        case x => StatementError("%s is not a call".format(x.description))
      }
    )

  // An optional step size for loops.
  def by: Parser[Option[Expr]] = opt("by" ~> expr)

  // A block of statements for a loop.
  def block: Parser[List[Statement]] = "{" ~> statements <~ "}"

  // A for loop.
  def loop: Parser[Statement] =
    positioned(
      "for" ~> variable ~ "over" ~ "[" ~ expr ~ "," ~ expr ~ """[])]""".r ~ by ~ block ^^ {
        case v@Variable(name) ~ _ ~ _ ~ start ~ "," ~ end ~ mode ~ step ~ block =>
          Loop(name, start, end, step, mode == "]", block)
        case x ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ _  =>
          StatementError("%s is not a variable".format(x.description))
        case _ => throw new Exception("unreachable")
      }
    )

  /*
   * Processes program into a Try[List[Statement]].
   */
  def apply(program: Reader): Try[List[Statement]] = parseAll(statements, program) match {
    case Success(e, _)   => Return(e)
    case Failure(msg, _) => Throw(new Exception(msg))
    case Error(msg, _)   => Throw(new Exception(msg))
  }
}

/*
 * Parser provides parsing and validation of sisbot programs.
 */
object Parser
{
  /*
   * Parses the given program by wrapping it in a StringReader.
   */
  def apply(program: String): Try[List[Statement]] = apply(new StringReader(program))

  /*
   * Parses the given program.
   */
  def apply(program: Reader): Try[List[Statement]] = {
    val parser = new Parser()
    parser(program)
  }

  /*
   * Given a parsed program, validates the Statements.
   */
  def validate(statements: List[Statement]): Try[List[Statement]] = {
    val ctxt = new ValidatingContext()
    Try.collect(statements.map { _.execute(ctxt) }).map { _ => statements }
  }

  def errorWithPos[T, T2](msg: String, pos: Position, t: Option[Throw[T2]] = None): Throw[T] = {
    val formattedMsg = "%s at line %d, column %s".format(msg, pos.line, pos.column)
    t match {
      case Some(t) => Throw(new Exception(formattedMsg, t.throwable))
      case None    => Throw(new Exception(formattedMsg))
    }
  }
}
