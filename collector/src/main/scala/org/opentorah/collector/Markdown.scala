package org.opentorah.collector

import com.vladsch.flexmark.ast.{Heading, Node}
import com.vladsch.flexmark.ext.toc.TocExtension
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.options.{DataHolder, MutableDataSet}
import org.opentorah.xml.{From, Xml}
import zio.Runtime
import scala.jdk.CollectionConverters.IterableHasAsJava
import java.io.InputStreamReader
import java.net.URL

// TODO escape HTML tags inside code fences - how?
final class Markdown(
  val title: Option[String],
  ast: Node
) {
  // TODO if anything gets cached, it should probably be the rendered HTML?
  def html: Xml.Element = Markdown.render(ast)
}

object Markdown {

  def load(url: URL): Markdown = {
    val ast: Node = parse(url)
    val heading: Option[Heading] = {
      val firstChild = ast.getFirstChild
      if (!firstChild.isInstanceOf[Heading]) None else {
        val result: Heading = firstChild.asInstanceOf[Heading]
        if (result.getLevel != 1) None else Some(result)
      }
    }

    // Note: heading is removed from the ast in-place!
    heading.foreach(_.unlink())

    new Markdown(
      title = heading.map(_.getText.toString),
      ast
    )
  }

  private def parse(url: URL): Node = parser.parseReader(new InputStreamReader(url.openStream()))
  //private def parse(file: File): Node = parser.parseReader(new FileReader(file))

  private def render(ast: Node): Xml.Element = {
    val html: String = renderer.render(ast)
    org.opentorah.xml.Parser.unsafeRun(From.string("flexmark", s"<div>$html</div>").load)
  }

  private lazy val options: DataHolder = {
    val result: MutableDataSet = new MutableDataSet()

    // uncomment to set optional extensions
    //result.set(Parser.EXTENSIONS, Arrays.asList(TablesExtension.create(), StrikethroughExtension.create()));

    // uncomment to convert soft-breaks to hard breaks
    //result.set(HtmlRenderer.SOFT_BREAK, "<br />\n");

    result
  }

  // You can re-use parser and renderer instances
  private lazy val parser: Parser = Parser
    .builder(options)
    .extensions(Seq(TocExtension.create()).asJava)
    .build()

  private lazy val renderer: HtmlRenderer = HtmlRenderer
    .builder(options)
    .extensions(Seq(TocExtension.create()).asJava)
    //    .escapeHtml(true)
    .build()
}
