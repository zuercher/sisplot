package us.zuercher.sisplot

import com.twitter.app.{App, Flag}
import com.twitter.util.{Return, Throw, Try}
import java.io

/*
 * Sisplot is the main entry point for the application.
 *
 * Usage:
 *   -help         Prints help message
 *   -normalize    Causes verts output to be normalized to a unit circle ([-1, 1]) with all angles
 *                   less than or equal to 2π.
 *   -output=FILE  Writes output to FILE instead of stdout.
 *   -svg          Generates an HTML-wrapped SVG for preview.
 *   -svg-size=N   Sets the SVG size to NxN (default is 1000).
 */
object Sisplot extends App
{
  val normalize = flag("normalize", false, "enabled normalization")
  val output = flag("output", "-", "set output file (- indicates stdout)")

  val svg = flag("svg", false, "enable SVG output (forces normalization)")
  val svgSize = flag("svg-size", 1000, "default size of exported SVG")
  val svgDrawUnitCircle = flag("svg-unit-circle", false, "Draw the unit circle on the SVG")

  def main() {
    val in =
      args.toSeq match {
        case Seq(file) => new io.FileReader(file)
        case Seq()     => new io.InputStreamReader(System.in)
        case _         =>
          System.err.println(flag.usage)
          System.exit(1)
          return
      }

    val out =
      output() match {
        case "-"  => new io.OutputStreamWriter(System.out)
        case path => new io.FileWriter(path)
      }

    val baseTarget =
      svg() match {
        case true  => new SVGRenderTarget(out, svgSize(), svgDrawUnitCircle())
        case false => new VertsRenderTarget(out)
      }

    val target =
      normalize() match {
        case true           => new NormalizingRenderTarget(baseTarget)
        case false if svg() => new NormalizingRenderTarget(baseTarget)
        case false          => baseTarget
      }

    execute(in, target).get
  }

  /*
   * Parses, validates, and executes the program from the given
   * Reader, outputing to the given RenderTarget. Returns any errors
   * as a Throw.
   */
  def execute(in: io.Reader, target: RenderTarget): Try[Unit] = {
    Parser(in).flatMap(Parser.validate).flatMap { statements =>
      val ctxt = new RuntimeContext(target)

      Try.collect { statements.map { _.execute(ctxt) } }
        .flatMap { _ => target.close() }
    }
  }
}
