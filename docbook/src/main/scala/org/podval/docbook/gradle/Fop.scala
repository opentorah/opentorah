package org.podval.docbook.gradle

import org.apache.fop.apps.{Fop, FopConfParser, FopFactory}
import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}

import javax.xml.transform.{Transformer, TransformerFactory}
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamSource


object Fop {
  def transform(input: File, configuration: File, output: File): Unit = {
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
}
