package org.podval.docbook.gradle

import java.io.File
import java.net.URL

import com.icl.saxon.TransformerFactoryImpl
import javax.xml.parsers.SAXParserFactory
import javax.xml.transform.Transformer
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.{StreamResult, StreamSource}
import org.apache.xerces.jaxp.SAXParserFactoryImpl
import org.gradle.api.{Action, DefaultTask, Project}
import org.gradle.api.provider.Property
import org.gradle.api.tasks.{InputDirectory, InputFile, OutputDirectory, OutputFile, TaskAction}
import org.podval.docbook.gradle.DocBookPlugin._
import org.xml.sax.InputSource

class SaxonTask extends DefaultTask {
  @InputDirectory
  val xslDir: File = copyXslInto(getProject)

  @InputDirectory
  val docBookDir: File = copyDocBookInto(getProject)

  private val inputFileName: Property[String] = getProject.getObjects.property(classOf[String])
  def getInputFileName: Property[String] = inputFileName

  @InputFile
  private var stylesheet: File = _
  def setStylesheet(value: File): Unit = stylesheet = value

  @OutputDirectory
  private var outputDirectory: File = _
  def getOutputDirectory: File = outputDirectory
  def setOutputDirectory(value: File): Unit = outputDirectory = value

  @OutputFile
  private var output: File = _
  def getOutput: File = output
  def setOutput(value: File): Unit = output = value

  @TaskAction
  def transform(): Unit = {
    //      // suppress output from the XSLT transforms - unless running with '-d' or '-i'.
    //      switch (project.gradle.startParameter.logLevel) {
    //        case LogLevel.DEBUG:
    //        case LogLevel.INFO:
    //        break;
    //        default:
    //          logging.captureStandardOutput(LogLevel.INFO)
    //        logging.captureStandardError(LogLevel.INFO)
    //      }

    println(s"inputFileName.get=${inputFileName.get}")
    val input = file(copyDocBookInto(getProject), inputFileName.get, "xml")
    SaxonTask.transform(input, stylesheet, output)
  }
}


object SaxonTask {
  def apply(
    project: Project,
    name: String,
    inputName: Property[String],
    stylesheetName: String,
    outputName: Option[String] = None,
    outputType: String): SaxonTask =
  {
    project.getTasks.create(name, classOf[SaxonTask], new Action[SaxonTask] {
      override def execute(transform: SaxonTask): Unit = {
        val outputDirectory: File = buildDirectory(project, outputType)
        // TODO check that inputFile exists and is a file
        transform.getInputFileName.set(inputName)
        transform.setStylesheet(xslFile(project, stylesheetName))
        transform.setOutputDirectory(outputDirectory)
        transform.setOutput(file(outputDirectory, outputName.getOrElse(inputName.get), outputType))
      }
    })
  }

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

  // TODO eliminate: we have the outputName!
  private def dropExtension(name: String): String = name.substring(0, name.lastIndexOf('.'))
}
