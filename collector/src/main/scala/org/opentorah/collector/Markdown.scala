package org.opentorah.collector

import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.ast.Node
import com.vladsch.flexmark.util.options.{DataHolder, MutableDataSet}
import java.io.{File, FileReader}

import scala.xml.Elem

object Markdown {

  def toHtml(file: File): Elem = {
    val ast: Node = parse(file)
    ???
  }

  def test(file: File): Unit = {
    val document: Node = parse(file)
    val html: String = renderer.render(document)
    println(html);
  }

  private def parse(file: File): Node = parser.parseReader(new FileReader(file))

  private lazy val options: DataHolder = {
    val result: MutableDataSet = new MutableDataSet()

    // uncomment to set optional extensions
    //result.set(Parser.EXTENSIONS, Arrays.asList(TablesExtension.create(), StrikethroughExtension.create()));

    // uncomment to convert soft-breaks to hard breaks
    //result.set(HtmlRenderer.SOFT_BREAK, "<br />\n");

    result
  }

  // You can re-use parser and renderer instances
  private lazy val parser: Parser = Parser.builder(options).build()
  private lazy val renderer: HtmlRenderer = HtmlRenderer.builder(options).build()
}
