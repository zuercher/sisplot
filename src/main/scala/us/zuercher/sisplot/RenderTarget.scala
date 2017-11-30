package us.zuercher.sisplot

import com.twitter.util.{Return, Throw, Try}
import java.io.Writer

/*
 * RenderTarget represents the target for a sisbot program's
 * execution. All rendered vertices are sent to a RenderTarget.  The
 * RenderTarget must be closed when the program completes.
 */
sealed trait RenderTarget extends Function2[Double, Double, Try[Unit]]
{
  def close(): Try[Unit]
}

/*
 * VertsRenderTarget writes vertices to a sisbot verts file via given
 * Writer.
 */
class VertsRenderTarget(w: Writer) extends RenderTarget
{
  override def apply(r: Double, theta: Double): Try[Unit] = {
    Try {
      w.write("%.5f %.5f\n".format(theta, r))
    }
  }

  override def close(): Try[Unit] = Try { w.flush() }
}

/*
 * BufferingRenderTarget buffers rendered vertices until close is invoked.
 */
abstract class BufferingRenderTarget(val renderTarget: RenderTarget) extends RenderTarget
{
  type Vertex = (Double, Double)

  protected var buffer = List.empty[Vertex]

  override def apply(r: Double, theta: Double): Try[Unit] = {
    buffer = buffer :+ (r, theta)
    Return.Unit
  }

  override def close(): Try[Unit] = {
    Try.collect {
      buffer.map { case (r, theta) => renderTarget(r, theta) }
    }.flatMap { _ =>
      renderTarget.close()
    }
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
    val maxR = buffer.map { case (r, _) => math.abs(r) }.max
    val normalizeFactor = if (maxR > 0.0) maxR else 1.0

    buffer = buffer.map { case (r, theta) =>
      val normalizedR = math.min(1.0, math.max(-1.0, r / normalizeFactor))

      (normalizedR, theta)
    }

    super.close()
  }
}

object SVGRenderTarget
{
  private val header = """<html><body><svg height="%d" width="%d"><polyline points=""""
  private val footer = """" style="fill:none;stroke:black;stroke-width:1" /></svg></body></html>"""
}

/*
 * SVGRenderTarget writes vertices to an HTML file containing an SVG
 * for the purposes of previewing sibot output. Vertices are converted
 * to cartesian coordinates with r = 0 at the center of a square
 * drawing. The SVGRenderTarget assumes normalized input (see
 * NormalizingRenderTarget).
 */
class SVGRenderTarget(w: Writer, size: Int) extends RenderTarget
{
  import SVGRenderTarget._

  private var firstPoint = true
  private var closed = false

  override def apply(r: Double, theta: Double): Try[Unit] = {
    if (math.abs(r) > 1.0) {
      return Throw(new Exception("SVG rendering requires normalized input"))
    }

    Try {
      val scale = size.toDouble / 2.0

      // Normalized r has the range [-1, 1], so multiply by scale factor
      // to get range [-scale, scale]. Add scale to get [0, 2*scale].
      // Since scale = size / 2, the final range is [0, size].

      // cos(theta) = x / r
      // x = r * cos(theta)
      val x = (r * scale * math.cos(theta)) + scale

      // sin(theta) = y / r
      val y = (r * scale * math.sin(theta)) + scale

      if (firstPoint) {
        w.write(header.format(size, size))
        firstPoint = false
      } else {
        w.write(" ")
      }

      w.write("%.5f,%.5f".format(x, y))
    }
  }

  override def close(): Try[Unit] = {
    Try {
      if (!closed) {
        w.write(footer)
        closed = true
      }
      w.flush()
    }
  }
}
