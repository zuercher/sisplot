package us.zuercher.sisplot

import com.twitter.util.Return
import java.io.{File, StringReader, StringWriter}
import org.scalatest.{FunSpec, Matchers}
import scala.io.Source

object Examples
{
  val All = {
    import scala.collection.JavaConverters._

    Thread.currentThread().getContextClassLoader().getResources("examples").asScala.map { url =>
      new File(url.getPath)
    }.collect {
      case file if file.isDirectory() => file
    }.flatMap {
      _.listFiles()
    }.map { resourceFile =>
      val name = "%s/%s".format(resourceFile.getParentFile().getName(), resourceFile.getName())
      name -> Source.fromFile(resourceFile.getPath).mkString
    }.toMap
  }
}

class ExamplesTest extends FunSpec with Matchers
{
  describe("Examples") {
    Examples.All.foreach { case (name, example) =>
      it("should execute %s".format(name)) {
        val in = new StringReader(example)
        val out = new StringWriter

        Sisplot.execute(in, new VertsRenderTarget(out)) should equal(Return.Unit)

        val verts = out.toString()
        verts.split("\n").length should be > 0
      }
    }
  }
}
