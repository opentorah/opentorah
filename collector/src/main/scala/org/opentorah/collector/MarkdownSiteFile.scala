package org.opentorah.collector

import java.io.{File, FileReader}
import com.vladsch.flexmark.ast.{Heading, Node}
import com.vladsch.flexmark.ext.toc.TocExtension
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.options.{DataHolder, MutableDataSet}
import org.opentorah.xml.From
import zio.Runtime
import scala.collection.JavaConverters.asJavaIterableConverter
import scala.xml.Elem

abstract class MarkdownSiteFile(val name: String, mdFile: File, override val url: Seq[String]) extends SiteFile {

  override def viewer: Viewer = Viewer.Collection

  final override protected def navigationLinks: Seq[NavigationLink] = Seq.empty

  override protected def titleAndContent: TitleAndContent = {
    val ast: Node = MarkdownSiteFile.parse(mdFile)
    val heading: Option[Heading] = {
      val firstChild = ast.getFirstChild
      if (!firstChild.isInstanceOf[Heading]) None else {
        val result: Heading = firstChild.asInstanceOf[Heading]
        if (result.getLevel != 1) None else Some(result)
      }
    }
    val title: Option[String] = heading.map(_.getText.toString)
    // Remove title from the AST (modification in-place!)
    heading.foreach(_.unlink())

    new TitleAndContent(
      title,
      content = MarkdownSiteFile.render(ast)
    )
  }
}

object MarkdownSiteFile {

  private def parse(file: File): Node = parser.parseReader(new FileReader(file))

  def render(ast: Node): Elem = {
    val html: String = renderer.render(ast)
    Runtime.default.unsafeRun(From.string("flexmark", s"<div>$html</div>").load)
  }

  def test(file: File): String = {
    val document: Node = parse(file)
    renderer.render(document)
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
