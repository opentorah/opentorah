package org.opentorah.markdown

import org.opentorah.collector.Site
import org.opentorah.xml.Xml
import org.podval.tools.run.Run
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URL

final class MarkdownTest extends AnyFlatSpec with Matchers {

  "Markdown" should "work" in {
    val markdown: Markdown = Markdown(new URL(s"file:///${Run.getProjectRoot}/collector/src/test/resources/fences.md"))
    val xml: Xml.Element = markdown.content
    println(xml)
    val string: String = Site.htmlPrettyPrinter.render(element = xml)
    println(string)
  }
}
