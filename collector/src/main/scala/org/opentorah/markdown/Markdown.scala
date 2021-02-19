package org.opentorah.markdown

import com.vladsch.flexmark.ast.{Heading, Node}
import com.vladsch.flexmark.ext.toc.TocExtension
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.options.{DataHolder, MutableDataSet}
import org.opentorah.util.Effects
import org.opentorah.xml.{From, Xml}
import java.io.InputStreamReader
import java.net.URL
import scala.jdk.CollectionConverters._

final class Markdown(
  val title: Option[String],
  val content: Xml.Element
)

object Markdown {

  def apply(url: URL): Markdown = {
    val node: Node = parse(url)

    val heading: Option[Heading] = {
      val firstChild: Node = node.getFirstChild
      if (!firstChild.isInstanceOf[Heading]) None else {
        val result: Heading = firstChild.asInstanceOf[Heading]
        if (result.getLevel != 1) None else Some(result)
      }
    }

    // Note: heading is removed from the ast in-place!
    heading.foreach(_.unlink)

    new Markdown(
      title = heading.map(_.getText.toString),
      content = render(node)
    )
  }

  private def parse(url: URL): Node = parser.parseReader(new InputStreamReader(url.openStream()))

  private def render(ast: Node): Xml.Element = Effects.unsafeRun(
    From.string("flexmark", s"<div>${renderer.render(ast)}</div>").load
  )

  private lazy val options: DataHolder = {
    val result: MutableDataSet = new MutableDataSet

    // uncomment to set optional extensions
    //result.set(Parser.EXTENSIONS, Arrays.asList(TablesExtension.create, StrikethroughExtension.create));

    // uncomment to convert soft-breaks to hard breaks
    //result.set(HtmlRenderer.SOFT_BREAK, "<br />\n");

    result
  }

  // You can re-use parser and renderer instances
  private lazy val parser: Parser = Parser
    .builder(options)
    .extensions(Seq(
      TocExtension.create
    ).asJava)
    .build

  private lazy val renderer: HtmlRenderer = HtmlRenderer
    .builder(options)
    .extensions(Seq(
      TocExtension.create,
      DoubleEscapeHtmlRendererExtension.create
    ).asJava)
    .build
}
