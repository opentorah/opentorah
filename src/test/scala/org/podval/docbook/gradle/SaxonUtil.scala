package org.podval.docbook.gradle

import java.io.{ByteArrayOutputStream, File, InputStream}

import javax.xml.transform.stream.StreamResult
import org.xml.sax.InputSource

object SaxonUtil {
  def toHtml(resourceName: String, substitutions: Map[String, String]): String = {
    val saxon: Saxon = new Saxon(
      xslDirectory = new File("build/docBookXsl/docbook").getAbsoluteFile,
      entities = Map.empty,
      substitutions = substitutions,
      dataDirectory = new File("/tmp/data"),
      logger = new Logger.TestLogger
    )

    val output = new ByteArrayOutputStream

    saxon.run(
      inputSource = getResourceAsInputSource(resourceName),
      stylesheetSource = saxon.resolve("http://docbook.sourceforge.net/release/xsl-ns/current/html/docbook.xsl"),
      outputTarget = new StreamResult(output),
      xslParameters = Map()
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
