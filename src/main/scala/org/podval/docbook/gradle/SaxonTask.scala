package org.podval.docbook.gradle

import org.gradle.api.{DefaultTask, Project}
import org.gradle.api.provider.{MapProperty, Property}
import org.gradle.api.tasks.{Input, InputDirectory, InputFile, OutputFile, TaskAction}
import java.io.{File, FileReader}
import java.net.URL

import javax.xml.parsers.SAXParserFactory
import javax.xml.transform.{Source, Transformer, URIResolver}
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.{StreamResult, StreamSource}
import com.icl.saxon.TransformerFactoryImpl
import org.apache.xerces.jaxp.SAXParserFactoryImpl
import org.xml.sax.{EntityResolver, InputSource, XMLReader}

import scala.beans.BeanProperty
import scala.collection.JavaConverters._

object SaxonTask {
  // If used in DocBook files, this prefix points to the data directory
  val docBookDataUrl: String = "http://podval.org/docbook/data/"

  def dataSystemId(systemId: String): Option[String] = drop(docBookDataUrl, systemId)

  // If used in DocBook files, those point to the DocBook XSL files.
  val docBookXslUrl: String = "http://podval.org/docbook/xsl/"
  val docBookXslUrlOfficial: String = "http://docbook.sourceforge.net/release/xsl-ns/current/"

  def docBookXslUrl(url: String): Option[String] = drop(docBookXslUrl, url)
    .orElse(drop(SaxonTask.docBookXslUrlOfficial, url))

  private def drop(what: String, from: String): Option[String] =
    if (from.startsWith(what)) Some(from.drop(what.length)) else None

  def apply(project: Project, name: String): SaxonTask = project.getTasks.create(name, classOf[SaxonTask])
}

class SaxonTask extends DefaultTask {
  @InputFile @BeanProperty val inputFile: Property[File] =
    getProject.getObjects.property(classOf[File])

  @InputFile @BeanProperty val stylesheetFile: Property[File] =
    getProject.getObjects.property(classOf[File])

  @Input @BeanProperty val xslParameters: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

  @InputDirectory @BeanProperty val xslDirectory: Property[File] =
    getProject.getObjects.property(classOf[File])

  @InputDirectory @BeanProperty val dataDirectory: Property[File] =
    getProject.getObjects.property(classOf[File])

  @InputDirectory @BeanProperty val imagesDirectory: Property[File] =
    getProject.getObjects.property(classOf[File])

  @OutputFile @BeanProperty val outputFile: Property[File] =
    getProject.getObjects.property(classOf[File])

  @TaskAction
  def saxon(): Unit = {
    outputFile.get.getParentFile.mkdirs

    val saxParserFactory: SAXParserFactory = new SAXParserFactoryImpl
    saxParserFactory.setXIncludeAware(true)
    val xmlReader: XMLReader = saxParserFactory.newSAXParser.getXMLReader
    xmlReader.setEntityResolver(mkEntityResolver(getProject))

    val stylesheetUrl: URL = stylesheetFile.get.toURI.toURL
    val transformer: Transformer = new TransformerFactoryImpl().newTransformer(
      new StreamSource(stylesheetUrl.openStream, stylesheetUrl.toExternalForm)
    )

    transformer.setURIResolver(mkUriResolver(getProject))

    val parameters: Map[String, String] = xslParameters.get.asScala.toMap

    def setParameter(name: String, value: String): Unit = {
      info(s"Transformer parameter $name=$value")
      transformer.setParameter(name, value)
    }

    def setOptionalParameter(name: String, value: String): Unit =
      if (!parameters.contains(name)) setParameter(name, value)

    parameters.foreach { case (name: String, value: String) => setParameter(name, value) }

    setOptionalParameter("img.src.path", imagesDirectory.get.toString)

    // Relevant only for HTML chunking:
    val outputFileName: String = {
      val result: String = outputFile.get.getName
      val lastDot: Int = result.lastIndexOf(".")
      result.substring(0, lastDot)
    }
    setOptionalParameter("root.filename", outputFileName)
    setOptionalParameter("base.dir", outputFile.get.getParent)

    transformer.transform(
      new SAXSource(
        xmlReader,
        new InputSource(inputFile.get.getAbsolutePath)
      ),
      new StreamResult(outputFile.get.getAbsolutePath)
    )
  }

  // Resolves references to data in DocBook files
  private def mkEntityResolver(project: Project): EntityResolver = new EntityResolver {
    override def resolveEntity(publicId: String, systemId: String): InputSource = {
      val result: Option[File] = SaxonTask.dataSystemId(systemId).map { path =>
        new File(dataDirectory.get, path)
      }

      info(s"publicId=$publicId; systemId=$systemId -> $result")

      result.map { file: File =>
        val source = new InputSource(new FileReader(file))
        source.setSystemId(systemId)
        source
      }.orNull
    }
  }

  // Resolves references to DocBook XSL in customization files
  private def mkUriResolver(project: Project): URIResolver = new URIResolver {
    override def resolve(href: String, base: String): Source = {
      val url: String = new URL(new URL(base), href).toString
      val result: Option[File] = SaxonTask.docBookXslUrl(url).map { path =>
        new File(xslDirectory.get, path)
      }

      info(s"href=$href; base=$base -> $result")

      result.map { file: File =>
        new StreamSource(file)
      }.orNull
    }
  }

  private def info(message: String): Unit = getLogger.info(message, null, null)
}
