package org.podval.docbook.gradle

import java.io.{ByteArrayOutputStream, File}
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamResult
import org.scalatest.{FunSpec, Matchers}

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
    val stylesheetSource: Source = Resolver.stringSource(???, None, Some(htmlStylesheetSystemId))

    val output: ByteArrayOutputStream = new ByteArrayOutputStream

    // TODO redo with Layout
    val resolver: Resolver = new Resolver(
      docBookXslDirectory = xslDirectory,
      entities = substitutions,
      dataDirectory = new File("/tmp/data"),
      logger: Logger
    )

    // TODO redo with layout
    Saxon.run(
      inputSource = Resolver.resourceInputSource("/" + resourceName),
      stylesheetSource = stylesheetSource,
      xslParameters = Map(),
      resolver = resolver,
      processingInstructionsSubstitutions = substitutions,
      outputTarget = new StreamResult(output),
      useXslt2 = false,
      logger = logger
    )

    output.toString
  }
}
