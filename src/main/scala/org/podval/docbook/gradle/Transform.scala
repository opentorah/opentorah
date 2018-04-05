package org.podval.docbook.gradle

import com.icl.saxon.TransformerFactoryImpl
import org.apache.fop.apps.{Fop, FopConfParser, FopFactory}
import org.apache.xerces.jaxp.SAXParserFactoryImpl
import org.xml.sax.InputSource
import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import java.net.URL

import javax.xml.parsers.SAXParserFactory
import javax.xml.transform.{Transformer, TransformerFactory}
import javax.xml.transform.sax.{SAXResult, SAXSource}
import javax.xml.transform.stream.{StreamResult, StreamSource}


object Transform {
  def docbook(input: File, stylesheet: File, output: File): Unit = {
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

  def fo(input: File, configuration: File, output: File): Unit = {
    output.getParentFile.mkdirs

    val outputStream: OutputStream = new BufferedOutputStream(new FileOutputStream(output))
    val fopFactory: FopFactory = new FopConfParser(configuration).getFopFactoryBuilder.build
    val fop: Fop = fopFactory.newFop("application/pdf", outputStream)

    try {
      val transformer: Transformer = TransformerFactory.newInstance().newTransformer()

      transformer.transform(
        new StreamSource(input),
        new SAXResult(fop.getDefaultHandler)
      )
    } finally {
      outputStream.close()
    }
  }

  private def dropExtension(name: String): String = name.substring(0, name.lastIndexOf('.'))
}
