package org.podval.docbook.gradle

import org.gradle.api.DefaultTask
import org.gradle.api.provider.Property
import org.gradle.api.tasks.{InputFile, OutputFile, TaskAction}
import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import javax.xml.transform.{Transformer, TransformerFactory}
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamSource
import org.apache.fop.apps.{Fop, FopConfParser, FopFactory}

import scala.beans.BeanProperty

class FopTask extends DefaultTask {
  @InputFile @BeanProperty val inputFile: Property[File] =
    getProject.getObjects.property(classOf[File])

  @InputFile @BeanProperty val configurationFile: Property[File] =
    getProject.getObjects.property(classOf[File])

  @OutputFile @BeanProperty val outputFile: Property[File] =
    getProject.getObjects.property(classOf[File])

  @TaskAction
  def fop(): Unit = {
    outputFile.get.getParentFile.mkdirs

    val outputStream: OutputStream = new BufferedOutputStream(new FileOutputStream(outputFile.get))

    val fopFactory: FopFactory = new FopConfParser(configurationFile.get).getFopFactoryBuilder.build
    val fop: Fop = fopFactory.newFop("application/pdf", outputStream)

    try {
      val transformer: Transformer = TransformerFactory.newInstance().newTransformer()

      transformer.transform(
        new StreamSource(inputFile.get),
        new SAXResult(fop.getDefaultHandler)
      )
    } finally {
      outputStream.close()
    }
  }
}
