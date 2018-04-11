package org.podval.docbook.gradle

import com.icl.saxon.TransformerFactoryImpl
import javax.xml.parsers.SAXParserFactory
import javax.xml.transform.Transformer
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.{StreamResult, StreamSource}
import org.apache.xerces.jaxp.SAXParserFactoryImpl
import org.gradle.api.{DefaultTask, Project}
import org.gradle.api.provider.{Property, Provider}
import org.gradle.api.tasks.{Input, InputDirectory, InputFile, OutputDirectory, OutputFile, TaskAction}
import org.xml.sax.{InputSource, XMLReader}
import java.io.File
import java.net.URL

object SaxonTask {
  def apply(
    project: Project,
    name: String,
    description: String,
    outputType: String,
    inputFileName: Provider[String],
    stylesheetName: String,
    outputFileNameOverride: Option[String] = None
  ): SaxonTask = project.getTasks.create(name, classOf[SaxonTask], (task: SaxonTask) => {
    task.setDescription(description)
    task.outputType.set(outputType)
    task.inputFileName.set(inputFileName)
    task.stylesheetName.set(stylesheetName)
    task.outputFileNameOverride.set(outputFileNameOverride)
  })
}

class SaxonTask extends DefaultTask {
  @InputDirectory
  val inputDirectory: File = DocBookPlugin.docBookDir(getProject)

  @Input
  val inputFileName: Property[String] = getProject.getObjects.property(classOf[String])

  @InputFile
  val inputFile: Provider[File] = inputFileName.map(DocBookPlugin.file(inputDirectory, _, "xml"))

  @InputDirectory
  val xslDir: File = DocBookPlugin.xslDir(getProject)

  @Input
  val stylesheetName: Property[String] = getProject.getObjects.property(classOf[String])

  @InputFile
  val stylesheetFile: Provider[File] = stylesheetName.map(DocBookPlugin.xslFile(getProject, _))

  @Input
  val outputType: Property[String] = getProject.getObjects.property(classOf[String])

  @OutputDirectory
  val outputDirectory: Provider[File] = outputType.map(DocBookPlugin.outputDirectory(getProject, _))
  def getOutputDirectory: Provider[File] = outputDirectory

  @Input
  val outputFileNameOverride: Property[Option[String]] = getProject.getObjects.property(classOf[Option[String]])

  @Input
  val outputFileName: Provider[String] = outputFileNameOverride.map(_.getOrElse(inputFileName.get))

  @OutputFile
  val outputFile: Provider[File] = outputFileName.map(DocBookPlugin.file(outputDirectory.get, _, outputType.get))

  @TaskAction
  def saxon(): Unit = {
    val input: File = inputFile.get
    val stylesheet: File = stylesheetFile.get
    val output: File = outputFile.get

    outputDirectory.get.mkdirs

    val saxParserFactory: SAXParserFactory = new SAXParserFactoryImpl
    saxParserFactory.setXIncludeAware(true)
    val xmlReader: XMLReader = saxParserFactory.newSAXParser.getXMLReader
    // TODO xmlReader.setEntityResolver()

    val stylesheetUrl: URL = stylesheet.toURI.toURL
    val transformer: Transformer = new TransformerFactoryImpl().newTransformer(
      new StreamSource(stylesheetUrl.openStream, stylesheetUrl.toExternalForm)
    )

    // TODO transformer.setURIResolver()

    transformer.setParameter("root.filename", outputFileName.get)
    transformer.setParameter("base.dir", outputDirectory.get + File.separator)

    transformer.transform(
      new SAXSource(
        xmlReader,
        new InputSource(input.getAbsolutePath)
      ),
      new StreamResult(output.getAbsolutePath)
    )
  }
}
