package org.podval.docbook.gradle

import org.gradle.api.Project
import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.{Transformer, TransformerFactory}
import javax.xml.transform.stream.StreamSource
import org.apache.fop.apps.{Fop, FopConfParser, FopFactory}

object DocBook2Pdf extends DocBook2 {
  override def saxon2intermediate: Boolean = true

  override def saxonOutputFormat: String = "fo"

  override def finalOutputFormat: String = "pdf"

  override protected def postProcess(
    layout: Layout,
    inputFileName: String,
    substitutions: Map[String, String],
    project: Project,
    logger: Logger
  ): Unit = {
    val inputFile: File = intermediateOutputFile(layout, inputFileName)
    val outputFile: File = finalOutputFile(layout, inputFileName)

    logger.info(s"Transforming $inputFile to $outputFile")
    val outputStream: OutputStream = new BufferedOutputStream(new FileOutputStream(outputFile))

    val fopConfParser: FopConfParser = new FopConfParser(
      layout.fopConfigurationFile,
      intermediateOutputDirectory(layout).toURI
    )

    val fopFactory: FopFactory = fopConfParser.getFopFactoryBuilder.build
    val fop: Fop = fopFactory.newFop("application/pdf", outputStream)

    try {
      val transformer: Transformer = TransformerFactory.newInstance().newTransformer()

      transformer.transform(
        new StreamSource(inputFile),
        new SAXResult(fop.getDefaultHandler)
      )
    } finally {
      outputStream.close()
    }
  }
}
