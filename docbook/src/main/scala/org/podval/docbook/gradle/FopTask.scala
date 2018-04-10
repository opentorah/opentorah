package org.podval.docbook.gradle

import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}

import javax.xml.transform.{Transformer, TransformerFactory}
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamSource
import org.apache.fop.apps.{Fop, FopConfParser, FopFactory}
import org.gradle.api.DefaultTask
import org.gradle.api.tasks.{InputFile, OutputFile, TaskAction}

class FopTask extends DefaultTask {
  @InputFile
  private var input: File = _
  def setInput(value: File): Unit = input = value

  @OutputFile
  private var output: File = _
  def setOutput(value: File): Unit = output = value

  @TaskAction
  def transform(): Unit = FopTask.transform(input, DocBookPlugin.fopConfiguration(getProject), output)
}


object FopTask {
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
