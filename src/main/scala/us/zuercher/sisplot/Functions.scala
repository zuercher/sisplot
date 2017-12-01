package us.zuercher.sisplot

import com.twitter.util.{Return, Throw, Try}
import scala.math

/*
 * Functions provides all the predefined functions available in the sisplot language.
 */
object Functions
{
  trait F extends Function2[Context, Seq[Double], Try[Double]] {
    def numArgs: Int
  }

  private def adapt1(f: Function1[Double, Double]): F = {
    new F {
      override val numArgs = 1
      override def apply(ctxt: Context, args: Seq[Double]) =
        require1(args).flatMap { arg => Try { f(arg) } }
    }
  }

  private def adapt2(f: Function2[Double, Double, Double]): F = {
    new F {
      override val numArgs = 2
      override def apply(ctxt: Context, args: Seq[Double]) =
        require2(args).flatMap { case (a, b) => Try { f(a,b) } }
    }
  }

  private def require1(args: Seq[Double]): Try[Double] = checkArgs(args, 1).map { _.head }

  private def require2(args: Seq[Double]): Try[(Double, Double)] = {
    checkArgs(args, 2).map { args => (args(0), args(1)) }
  }

  private def require3(args: Seq[Double]): Try[(Double, Double, Double)] = {
    checkArgs(args, 3).map { args => (args(0), args(1), args(2)) }
  }

  private val render = new F {
    override val numArgs = 2
    override def apply(ctxt: Context, args: Seq[Double]) =
      require2(args).flatMap { case (r, theta) => ctxt.render.vertex(r, theta).map { _ => 0.0 } }
  }

  // Make use of the fact that sisbot will draw a smooth curve at the given radius.
  private val render_arc = new F {
    override val numArgs = 3
    override def apply(ctxt: Context, args: Seq[Double]) = {
      require3(args).flatMap { case (r, theta, sweep) =>
        ctxt.render.arc(r, theta, sweep).map { _ => 0.0 }
      }
    }
  }

  private val funcs = Map[String, F](
    // one arg funcs
    "cos" -> adapt1(math.cos),
    "sin" -> adapt1(math.sin),
    "tan" -> adapt1(math.tan),
    "acos" -> adapt1(math.acos),
    "asin" -> adapt1(math.asin),
    "atan" -> adapt1(math.atan),
    "cosh" -> adapt1(math.cosh),
    "sinh" -> adapt1(math.sinh),
    "tanh" -> adapt1(math.tanh),
    "abs" -> adapt1(math.abs),
    "ceil" -> adapt1(math.ceil),
    "floor" -> adapt1(math.floor),
    "sqrt" -> adapt1(math.sqrt),
    "ln" -> adapt1(math.log),
    "log10" -> adapt1(math.log10),

    // two arg funcs
    "render" -> render,
    "pow" -> adapt2(math.pow),
    "min" -> adapt2(math.min),
    "max" -> adapt2(math.max),

    // three arg funcs
    "render_arc" -> render_arc,
  )

  /* AllNames is a set of all available function names. */
  val AllNames: Set[String] = funcs.keys.toSet

  /*
   * NumArgsFor returns the number of arguments required by the given
   * function or an error if no such function exists.
   */
  def NumArgsFor(name: String): Try[Int] = {
    funcs.get(name) match {
      case Some(f) => Return(f.numArgs)
      case None    => noSuchFunction(name)
    }
  }

  /*
   * Validates that the named function exists and takes the given number of arguments.
   */
  def validate(ctxt: Context, name: String, args: Seq[Double]): Try[Unit] = {
    funcs.get(name) match {
      case Some(f) if f.numArgs != args.length => wrongNumberOfArguments(args.length, f.numArgs)
      case Some(f)                             => Return.Unit
      case None                                => noSuchFunction(name)
    }
  }

  /*
   * Invokes the named function with the given arguments.
   */
  def dispatch(ctxt: Context, name: String, args: Seq[Double]): Try[Double] = {
    funcs.get(name) match {
      case Some(f) => f(ctxt, args)
      case None    => noSuchFunction(name)
    }
  }

  private def noSuchFunction[T](name: String): Throw[T] = {
    Throw(new Exception("%s: no such function".format(name)))
  }

  private def wrongNumberOfArguments[T](got: Int, want: Int): Throw[T] = {
    Throw(
      new Exception("wrong number of arguments: requires %d, got %d".format(want, got))
    )
  }

  private def checkArgs(args: Seq[Double], n: Int): Try[Seq[Double]] = {
    if (args.length != n) {
      return wrongNumberOfArguments(args.length, n)
    }

    Return(args)
  }
}
