package us.zuercher.sisplot

import com.twitter.util.{Return, Throw, Try}

/*
 * Context represents the execution context of a sisplot program.
 */
trait Context
{
  /*
   *  Returns the current value of the named variable. It fails if the
   *  variable has not been defined.
   */
  def valueOf(name: String): Try[Double]

  /*
   *  Assigns the given value to the named variable.
   */
  def assign(name: String, value: Double): Unit

  /*
   * Generates an Iterator[Double] over the given range. If step is
   * provided, the values increase (or decrease) by that
   * amount. Otherwise a default step size is used. If inclusive is
   * true, the iterator terminates when the next value would be past
   * the end value. Otherwise, it terminates when the next value would
   * be past or equal to the end value.
   */
  def range(start: Double, end: Double, step: Option[Double], inclusive: Boolean): Iterator[Double]

  /*
   * Returns the current RenderTarget.
   */
  def render: RenderTarget

  /*
   * Invoke the given function with the given args. Failures include
   * specifying the wrong number of arguments to the function.
   */
  def dispatch(name: String, args: Seq[Double]): Try[Double]
}

/*
 * ContextBase is an abstract base class for Context implementations.
 * It provides variable assignment, variable lookup and function call
 * dispatch.
 */
abstract class ContextBase extends Context
{
  private var vars = Map[String, Double]()

  override def valueOf(name: String): Try[Double] = {
    vars.get(name) match {
      case Some(value) => Return(value)
      case None        => Throw(new Exception("undefined variable %s".format(name)))
    }
  }

  override def assign(name: String, value: Double) {
    if (name != "_") {
      vars = vars + (name -> value)
    }
  }

  override def dispatch(name: String, args: Seq[Double]): Try[Double] =
    Functions.dispatch(this, name, args)
}

/*
 * ValidatingContext extends ContextBase to provide a program
 * validation Context.  All render operations succeed during
 * validation. Range operations return only the initial value to speed
 * execution.
 */
class ValidatingContext extends ContextBase
{
  override def range(
    start: Double,
    end: Double,
    step: Option[Double],
    inclusive: Boolean
  ): Iterator[Double] = {
    new Iterator[Double] {
      private var done = false

      override def hasNext = !done

      override def next() = {
        if (done) {
          throw new NoSuchElementException()
        }
        done = true
        start
      }
    }
  }

  override val render = NoopRenderTarget
}

/*
 * RuntimeContext extends ContextBase to provide a program execution
 * Context.
 */
class RuntimeContext(val renderTarget: RenderTarget) extends ContextBase
{
  private val DefaultStep = 0.01

  private class Iter(start: Double, val end: Double, val step: Double) extends Iterator[Double]
  {
    protected var curr = start

    override def hasNext = curr < end
    override def next() = {
      if (!hasNext) {
        throw new NoSuchElementException()
      }
      val v = curr
      curr += step
      v
    }
  }

  private class IncIter(start: Double, end: Double, step: Double) extends Iter(start, end, step)
  {
    override def hasNext = curr <= end
  }

  private class RevIter(start: Double, end: Double, step: Double) extends Iter(start, end, step)
  {
    override def hasNext = curr > end
  }

  private class IncRevIter(start: Double, end: Double, step: Double)
      extends Iter(start, end, step)
  {
    override def hasNext = curr >= end
  }

  override def range(
    start: Double,
    end: Double,
    step: Option[Double],
    inclusive: Boolean
  ): Iterator[Double] = {
    if (start == end) {
      return new Iter(start, end, step.getOrElse(DefaultStep))
    }

    if (start < end) {
      if (inclusive) {
        return new IncIter(start, end, step.getOrElse(DefaultStep))
      }
      return new Iter(start, end, step.getOrElse(DefaultStep))
    }

    if (inclusive) {
      return new IncRevIter(start, end, step.getOrElse(-DefaultStep))
    }

    return new RevIter(start, end, step.getOrElse(-DefaultStep))
  }

  override val render = renderTarget
}
