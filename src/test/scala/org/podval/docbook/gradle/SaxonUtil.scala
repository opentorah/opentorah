package org.podval.docbook.gradle

import java.io.{ByteArrayOutputStream, File, InputStream}
import java.net.URI

import javax.xml.transform.stream.StreamResult
import org.xml.sax.InputSource

object SaxonUtil {
  def toHtml(resourceName: String, substitutions: Map[String, String]): String = {
    val useDocBookXslt20: Boolean = false // TODO
    val xslDirectory = new File("build/docBookXsl/docbook").getAbsoluteFile
    val logger = new Logger.TestLogger
    val uriResolver = Saxon.getUriResolver(xslDirectory, logger)
    val stylesheetSource = uriResolver.resolve(new URI("http://docbook.sourceforge.net/release/xsl-ns/current/html/docbook.xsl"))
    val output: ByteArrayOutputStream = new ByteArrayOutputStream

    Saxon.run(
      inputSource = getResourceAsInputSource(resourceName),
      stylesheetSource = stylesheetSource,
      outputTarget = new StreamResult(output),
      xslParameters = Map(),
      entitySubstitutions = substitutions,
      processingInstructionsSubstitutions = substitutions,
      xslDirectory = xslDirectory,
      dataDirectory = new File("/tmp/data"),
      useDocBookXslt20 = useDocBookXslt20,
      logger = logger
    )

    output.toString
  }

  private def getResourceAsInputSource(name: String): InputSource = {
    val inputStream: InputStream = getClass.getResourceAsStream("/" + name)
    val result = new InputSource(name)
    result.setByteStream(inputStream)
    result
  }
}
