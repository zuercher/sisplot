package us.zuercher.sisplot

import com.twitter.util.{Return, Throw, Try}
import java.io.Writer

sealed trait RenderCmd
case class Vertex(r: Double, theta: Double) extends RenderCmd
case class Arc(r: Double, theta: Double, sweep: Double) extends RenderCmd

/*
 * RenderTarget represents the target for a sisbot program's
 * execution. All rendered vertices are sent to a RenderTarget.  The
 * RenderTarget must be closed when the program completes.
 */
trait RenderTarget
{
  def apply(cmd: RenderCmd): Try[Unit]

  def vertex(r: Double, theta: Double): Try[Unit] = apply(Vertex(r, theta))
  def arc(r: Double, theta: Double, sweep: Double): Try[Unit] = apply(Arc(r, theta, sweep))

  def close(): Try[Unit]
}

object NoopRenderTarget extends RenderTarget {
  def apply(cmd: RenderCmd) = Return.Unit
  def close() = Return.Unit
}

object FailingRenderTarget extends RenderTarget {
  def apply(cmd: RenderCmd) = Throw(new UnsupportedOperationException)
  def close() = Throw(new UnsupportedOperationException)
}

class CountingRenderTarget extends RenderTarget {
  private var counter = 0

  def count(): Int = counter
  def apply(cmd: RenderCmd) = {
    counter += 1
    Return.Unit
  }
  def close() = Return.Unit
}


/*
 * VertsRenderTarget writes vertices to a sisbot verts file via given
 * Writer.
 */
class VertsRenderTarget(w: Writer) extends RenderTarget
{
  override def apply(cmd: RenderCmd): Try[Unit] = {
    Try {
      cmd match {
        case Vertex(r, theta) =>
          w.write("%.5f %.5f\n".format(theta, r))
        case Arc(r, theta, sweep) =>
          w.write("%.5f %.5f\n".format(theta, r))
          w.write("%.5f %.5f\n".format(theta + sweep, r))
      }
    }
  }

  override def close(): Try[Unit] = Try { w.flush() }
}

/*
 * BufferingRenderTarget buffers rendered vertices until close is invoked.
 */
class BufferingRenderTarget(val renderTarget: RenderTarget) extends RenderTarget
{
  var buffer = List.empty[RenderCmd]

  override def apply(cmd: RenderCmd): Try[Unit] = {
    Try {
      buffer = buffer :+ cmd
    }
  }

  override def close(): Try[Unit] = {
    Try.collect {
      buffer.map(renderTarget(_))
    }.flatMap { _ => renderTarget.close() }
  }
}

/*
 * NormalizingRenderTarget extends BufferingRenderTarget to normalize
 * vertices before writing. All radii values are scaled to the unit
 * circle (such that -1 <= r <= 1).
 */
class NormalizingRenderTarget(renderTarget: RenderTarget)
    extends BufferingRenderTarget(renderTarget)
{
  override def close(): Try[Unit] = {
    val maxR = buffer.map {
      case Vertex(r, _) => math.abs(r)
      case Arc(r, _, _) => math.abs(r)
    }.max

    val normalizeFactor = if (maxR > 0.0) maxR else 1.0

    val normalize = (r: Double) => math.min(1.0, math.max(-1.0, r / normalizeFactor))

    buffer = buffer.map {
      case Vertex(r, theta)     => Vertex(normalize(r), theta)
      case Arc(r, theta, sweep) => Arc(normalize(r), theta, sweep)
    }

    super.close()
  }
}

object SVGRenderTarget
{
  private val header = """<html><body><svg height="%d" width="%d">""" + "\n"
  private val footer = """</svg></body></html>"""

  private[this] val pointsFmt =
    """<polyline points="%s" style="fill:none;stroke:%s;stroke-width:%d" />""" + "\n"

  private[this] val pathFmt =
    """<path d="%s" style="fill:none;stroke:%s;stroke-width:%d" />""" + "\n"

  private[this] val circleFmt =
    """<circle cx="%.5f" cy="%.5f" r="%.5f" style="fill:none;stroke:%s;stroke-width:%d" />""" + "\n"

  private[this] val defaultStrokeColor = "black"
  private[this] val defaultStrokeWidth = 5

  def path(
    cmd: String,
    strokeColor: String = defaultStrokeColor,
    strokeWidth: Int = defaultStrokeWidth
  ): String = {
    pathFmt.format(cmd, strokeColor, strokeWidth)
  }

  def circle(
    cx: Double,
    cy: Double,
    r: Double,
    strokeColor: String = defaultStrokeColor,
    strokeWidth: Int = defaultStrokeWidth
  ): String = {
    circleFmt.format(cx, cy, r, strokeColor, strokeWidth)
  }

  def points(
    p: Seq[(Double, Double)],
    strokeColor: String = defaultStrokeColor,
    strokeWidth: Int = defaultStrokeWidth
  ): String = {
    pointsFmt.format(
      p.map { case (x, y) => "%.5f,%.5f".format(x, y) }.mkString(" "),
      strokeColor,
      strokeWidth
    )
  }
}

/*
 * SVGRenderTarget writes vertices to an HTML file containing an SVG
 * for the purposes of previewing sibot output. Vertices are converted
 * to cartesian coordinates with r = 0 at the center of a square
 * drawing. The SVGRenderTarget assumes normalized input (see
 * NormalizingRenderTarget).
 */
class SVGRenderTarget(w: Writer, size: Int, drawUnitCircle: Boolean = false) extends RenderTarget
{
  val scale = size.toDouble / 2.0

  import SVGRenderTarget._

  private var firstCmd = true
  private var closed = false

  private var pointAccumulator = List.empty[(Double, Double)]

  override def apply(cmd: RenderCmd): Try[Unit] = {
    startOnce().flatMap { _ =>
      cmd match {
        case v: Vertex => render(v)
        case a: Arc => render(a)
      }
    }.onFailure { case t: Throwable => t.printStackTrace() }
  }

  def render(v: Vertex): Try[Unit] = {
    if (math.abs(v.r) > 1.0) {
      return Throw(new Exception("SVG rendering requires normalized input"))
    }

    Try {
      pointAccumulator = pointAccumulator :+ toCartesian(v.r, v.theta)
    }
  }

  def render(a: Arc): Try[Unit] = {
    if (math.abs(a.r) > 1.0) {
      return Throw(new Exception("SVG rendering requires normalized input"))
    }

    val (x, y) = toCartesian(a.r, a.theta)
    val (x2, y2) = toCartesian(a.r, a.theta + a.sweep)

    Try.collect {
      Seq(
        render(Vertex(a.r, a.theta)),
        Try {
          flushPoints(Some(x -> y))

          w.write(path(
            "M %.5f,%.5f A %.5f,%.5f 0 %d %d %.5f,%.5f".format(
              x, y,
              a.r * scale, a.r * scale,
              if (math.abs(a.sweep) <= math.Pi) 0 else 1,
              if (a.sweep < 0.0) 1 else 0,
              x2, y2,
            )
          ))

          pointAccumulator = pointAccumulator :+ (x2, y2)
        }
      )
    }.map{ _ => () }
  }

  def startOnce(): Try[Unit] = {
    if (!firstCmd) {
      return Return.Unit
    }

    firstCmd = false
    Try {
      w.write(header.format(size, size))
    }
  }

  def flushPoints(startPoint: Option[(Double, Double)]) {
    if (pointAccumulator.isEmpty) {
      return
    }

    startPoint.foreach { p =>
      pointAccumulator = pointAccumulator :+ p
    }

    w.write(points(pointAccumulator))
    pointAccumulator = List.empty
  }

  def toCartesian(r: Double, theta: Double): (Double, Double) = {
    // Normalized r has the range [-1, 1], so multiply by scale factor
    // to get range [-scale, scale]. Add scale to get [0, 2*scale].
    // Since scale = size / 2, the final range is [0, size].

    // cos(theta) = x / r
    // x = r * cos(theta)
    val x = (r * scale * math.cos(theta)) + scale

    // sin(theta) = y / r, but handle SVG y increasing down screen instead of up
    val y = -(r * scale * math.sin(theta)) + scale

    x -> y
  }

  override def close(): Try[Unit] = {
    Try {
      if (!closed) {
        flushPoints(None)

        if (drawUnitCircle) {
          w.write(circle(scale, scale, scale, "#cccccc", 1))

          for(i <- 0 until 128) {
            val color = i match {
              case 0 | 32 | 64 | 96 => "red"
              case _                => "#cccccc"
            }

            w.write(points(
              Seq(toCartesian(0, 0), toCartesian(1.0, math.Pi / 64.0 * i.toDouble)),
              color,
              1
            ))
          }
        }

        w.write(footer)
        closed = true
      }
      w.flush()
    }
  }
}
