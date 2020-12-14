package org.opentorah.collector

import org.opentorah.util.Files
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.podval.tools.run.Run

class MarkdownTest extends AnyFlatSpec with Matchers {

  "Markdown converter" should "work" in {
    val mdFile = Files.file(Run.getProjectRoot, Seq("collector", "src", "test", "resources", "about.md"))
    val html = MarkdownSiteFile.test(mdFile)
//    println(html)
  }
}
