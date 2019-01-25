package org.podval.docbook.gradle

import org.gradle.api.{Action, DefaultTask, Project}
import org.gradle.api.provider.{Property, Provider, MapProperty}
import org.gradle.api.tasks.{Input, InputDirectory, InputFile, OutputDirectory, OutputFile, TaskAction}

import java.io.{File, FileReader}
import java.net.URL
import javax.xml.parsers.SAXParserFactory
import javax.xml.transform.{Source, Transformer, URIResolver}
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.{StreamResult, StreamSource}
import com.icl.saxon.TransformerFactoryImpl
import org.apache.xerces.jaxp.SAXParserFactoryImpl
import org.xml.sax.{EntityResolver, InputSource, XMLReader}

import scala.collection.JavaConverters._

object SaxonTask {
  def apply(
    project: Project,
    name: String,
    description: String,
    outputType: String,
    inputFileName: Provider[String],
    stylesheetName: String,
    dataDirectory: Property[File],
    imagesDirectory: Property[File],
    xslParameters: MapProperty[String, String],
    outputFileNameOverride: Option[String] = None
  ): SaxonTask = project.getTasks.create(name, classOf[SaxonTask], new Action[SaxonTask] {
    override def execute(task: SaxonTask): Unit = {
      task.setDescription(description)
      task.setGroup("publishing")
      task.outputType.set(outputType)
      task.inputFileName.set(inputFileName)
      task.stylesheetName.set(stylesheetName)
      task.xslParameters.set(xslParameters)
      task.dataDirectory.set(dataDirectory)
      task.imagesDirectory.set(imagesDirectory)
      task.outputFileNameOverride.set(outputFileNameOverride)
    }})
}

class SaxonTask extends DefaultTask {
  @InputDirectory
  val inputDirectory: File = Locations.docBookDir(getProject)

  @Input
  val inputFileName: Property[String] = getProject.getObjects.property(classOf[String])

  @InputFile
  val inputFile: Provider[File] = inputFileName.map(Locations.file(inputDirectory, _, "xml"))

  @InputDirectory
  val xslDir: File = Locations.xslDir(getProject)

  @Input
  val stylesheetName: Property[String] = getProject.getObjects.property(classOf[String])

  @InputFile
  val stylesheetFile: Provider[File] = stylesheetName.map(Locations.xslFile(getProject, _))

  @Input
  val xslParameters: MapProperty[String, String] = getProject.getObjects.mapProperty(classOf[String], classOf[String])

  @InputFile
  val dataDirectory: Property[File] = getProject.getObjects.property(classOf[File])

  @InputFile
  val imagesDirectory: Property[File] = getProject.getObjects.property(classOf[File])

  @Input
  val outputType: Property[String] = getProject.getObjects.property(classOf[String])

  @OutputDirectory
  val outputDirectory: Provider[File] = outputType.map(Locations.outputDirectory(getProject, _))
  def getOutputDirectory: Provider[File] = outputDirectory

  @Input
  val outputFileNameOverride: Property[Option[String]] = getProject.getObjects.property(classOf[Option[String]])

  @Input
  val outputFileName: Provider[String] = outputFileNameOverride.map(_.getOrElse(inputFileName.get))

  @OutputFile
  val outputFile: Provider[File] = outputFileName.map(Locations.file(outputDirectory.get, _, outputType.get))

  @TaskAction
  def saxon(): Unit = {
    outputDirectory.get.mkdirs

    val saxParserFactory: SAXParserFactory = new SAXParserFactoryImpl
    saxParserFactory.setXIncludeAware(true)
    val xmlReader: XMLReader = saxParserFactory.newSAXParser.getXMLReader
    xmlReader.setEntityResolver(mkEntityResolver(getProject, dataDirectory.get))

    val stylesheetUrl: URL = stylesheetFile.get.toURI.toURL
    val transformer: Transformer = new TransformerFactoryImpl().newTransformer(
      new StreamSource(stylesheetUrl.openStream, stylesheetUrl.toExternalForm)
    )

    transformer.setURIResolver(mkUriResolver(getProject))

    val parameters: Map[String, String] = xslParameters.get.asScala.toMap

    def setParameter(name: String, value: String): Unit = {
      log(s"Transformer parameter $name=$value")
      transformer.setParameter(name, value)
    }

    def setOptionalParameter(name: String, value: String): Unit =
      if (!parameters.contains(name)) setParameter(name, value)

    parameters.foreach { case (name: String, value: String) => setParameter(name, value) }

    setOptionalParameter("img.src.path", imagesDirectory.get.toString)

    // For chunking:
    setOptionalParameter("root.filename", outputFileName.get)
    setOptionalParameter("base.dir", outputDirectory.get.toString)

    transformer.transform(
      new SAXSource(
        xmlReader,
        new InputSource(inputFile.get.getAbsolutePath)
      ),
      new StreamResult(outputFile.get.getAbsolutePath)
    )
  }

  // Resolves references to data in DocBook files
  private def mkEntityResolver(project: Project, dataDirectory: File): EntityResolver = new EntityResolver {
    override def resolveEntity(publicId: String, systemId: String): InputSource = {
      val result: Option[File] = drop(Locations.docBookDataUrl, systemId).map { path =>
        new File(dataDirectory, path)
      }

      log(s"publicId=$publicId; systemId=$systemId -> $result")

      result.map { file: File =>
        val source = new InputSource(new FileReader(file))
        source.setSystemId(systemId)
        source
      }.orNull
    }
  }

  // Resolves references to DocBook XSL in customization files
  private def mkUriResolver(project: Project): URIResolver = new URIResolver {
    private val xslDirectory: File = Locations.docBookXsl(project)

    override def resolve(href: String, base: String): Source = {
      val url: String = new URL(new URL(base), href).toString
      val result: Option[File] = drop(Locations.docBookXslUrl, url)
        .orElse(drop(Locations.docBookXslUrlOfficial, url)).map { path =>
        new File(xslDirectory, path)
      }

      log(s"href=$href; base=$base -> $result")

      result.map { file: File =>
        new StreamSource(file)
      }.orNull
    }
  }

  private def drop(what: String, from: String): Option[String] =
    if (from.startsWith(what)) Some(from.drop(what.length)) else None

  private def log(message: String): Unit = getLogger.info(message, null, null)
}
