package org.podval.docbook.gradle

import org.gradle.api.{Action, DefaultTask, Project}
import org.gradle.api.provider.{Property, Provider}
import org.gradle.api.tasks.{Input, InputFile, OutputDirectory, OutputFile, TaskAction}
import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import javax.xml.transform.{Transformer, TransformerFactory}
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamSource
import org.apache.fop.apps.{Fop, FopConfParser, FopFactory}

object FopTask {
  def apply(
    project: Project,
    name: String,
    description: String,
    inputFile: Provider[File],
    outputFileName: Property[String]
  ): FopTask = project.getTasks.create("docBookPdf", classOf[FopTask], new Action[FopTask] {
    override def execute(task: FopTask): Unit = {
      task.setDescription(description)
      task.setGroup("publishing")
      task.inputFile.set(inputFile)
      task.outputFileName.set(outputFileName)
    }})
}

class FopTask extends DefaultTask {
  @InputFile
  val inputFile: Property[File] = getProject.getObjects.property(classOf[File])

  @OutputDirectory
  val outputDirectory: File = Locations.outputDirectory(getProject, "pdf")

  @Input
  val outputFileName: Property[String] = getProject.getObjects.property(classOf[String])

  @OutputFile
  val outputFile: Provider[File] = outputFileName.map(Locations.file(outputDirectory, _, "pdf"))

  @TaskAction
  def fop(): Unit = {
    val input: File = inputFile.get
    val output: File = outputFile.get

    outputDirectory.mkdirs

    val outputStream: OutputStream = new BufferedOutputStream(new FileOutputStream(output))

    val configurationFile: File = Locations.fopConfiguration(getProject)

    val fopFactory: FopFactory = new FopConfParser(configurationFile).getFopFactoryBuilder.build
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
