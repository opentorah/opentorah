package org.podval.docbook.gradle

import java.io.{ByteArrayOutputStream, File}
import java.net.URI

import javax.xml.transform.stream.StreamResult
import org.scalatest.{FunSpec, Matchers}
import org.xml.sax.InputSource

class SaxonTest extends FunSpec with Matchers  {
  describe("Saxon") {
    val version: String = "blah-blah"
    toHtml("DocBookWithSubstitutions.xml", Map()).contains(version) shouldBe false
    toHtml("DocBookWithSubstitutions.xml", Map("version" -> version)).contains(version) shouldBe true
    println(toHtml("DocBookWithSubstitutions.xml", Map("version" -> version)))
  }

  private def toHtml(resourceName: String, substitutions: Map[String, String]): String = {
    val xslDirectory = new File("build/docBookXsl/docbook").getAbsoluteFile
    val logger = new Logger.TestLogger

    // TODO use string form of a trivial customization file that imports non-chunking HTML stylesheet instead:
    val htmlStylesheetSystemId = "http://docbook.sourceforge.net/release/xsl-ns/current/html/docbook.xsl"
    val stylesheetSource: Source = Xml.stringSource(???, null, htmlStylesheetSystemId)

    val output: ByteArrayOutputStream = new ByteArrayOutputStream

    Saxon.run(
      inputSource = Xml.resourceInputStream("/" + resourceName),
      stylesheetSource = stylesheetSource,
      outputTarget = new StreamResult(output),
      xslParameters = Map(),
      entitySubstitutions = substitutions,
      processingInstructionsSubstitutions = substitutions,
      xslDirectory = xslDirectory,
      dataDirectory = new File("/tmp/data"),
      useXslt2 = false,
      logger = logger
    )

    output.toString
  }
}
