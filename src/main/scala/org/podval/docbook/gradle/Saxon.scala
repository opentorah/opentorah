package org.podval.docbook.gradle

import java.io.File
import java.net.URL
import javax.xml.parsers.SAXParserFactory
import javax.xml.transform.Transformer
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.{StreamResult, StreamSource}

import com.icl.saxon.TransformerFactoryImpl
import org.apache.xerces.jaxp.SAXParserFactoryImpl
import org.xml.sax.InputSource

object Saxon {
  def transform(input: File, stylesheet: File, output: File): Unit = {
    output.getParentFile.mkdirs

    val saxParserFactory: SAXParserFactory = new SAXParserFactoryImpl
    saxParserFactory.setXIncludeAware(true)

    val stylesheetUrl: URL = stylesheet.toURI.toURL
    val transformer: Transformer = new TransformerFactoryImpl().newTransformer(
      new StreamSource(stylesheetUrl.openStream, stylesheetUrl.toExternalForm)
    )

    transformer.setParameter("root.filename", dropExtension(output.getName))
    transformer.setParameter("base.dir", output.getParent + File.separator)

    transformer.transform(
      new SAXSource(
        saxParserFactory.newSAXParser.getXMLReader,
        new InputSource(input.getAbsolutePath)
      ),
      new StreamResult(output.getAbsolutePath)
    )
  }

  private def dropExtension(name: String): String = name.substring(0, name.lastIndexOf('.'))
}
