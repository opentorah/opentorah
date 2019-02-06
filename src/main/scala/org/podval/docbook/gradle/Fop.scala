package org.podval.docbook.gradle

import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import javax.xml.transform.Transformer
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamSource
import org.apache.fop.apps.{FopConfParser, FopFactory}

object Fop {

  def run(
    configurationFile: File,
    inputFile: File,
    baseDirectory: File,
    outputFile: File,
    logger: Logger
  ): Unit = {
    logger.info(
      s"""Fop.run(
         |  configurationFile = $configurationFile,
         |  inputFile = $inputFile,
         |  baseDirectory = "$baseDirectory",
         |  outputFile = $outputFile,
         |)""".stripMargin
    )

    val fopConfParser: FopConfParser = new FopConfParser(
      configurationFile,
      baseDirectory.toURI
    )

    val fopFactory: FopFactory = fopConfParser.getFopFactoryBuilder.build

    val outputStream: OutputStream = new BufferedOutputStream(new FileOutputStream(outputFile))
    val fop: org.apache.fop.apps.Fop = fopFactory.newFop("application/pdf", outputStream)

    try {
      val transformer: Transformer = Saxon.getTransformerFactory.newTransformer

      transformer.transform(
        new StreamSource(inputFile),
        new SAXResult(fop.getDefaultHandler)
      )
    } finally {
      outputStream.close()
    }
  }
}
