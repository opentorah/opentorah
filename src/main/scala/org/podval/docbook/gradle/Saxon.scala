package org.podval.docbook.gradle

import com.icl.saxon.TransformerFactoryImpl
import java.io.File
import java.net.URI

import javax.xml.parsers.SAXParserFactory
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.{StreamResult, StreamSource}
import javax.xml.transform.{Result, Source, Transformer, TransformerFactory}
import org.apache.xerces.jaxp.SAXParserFactoryImpl
import org.xml.sax.{InputSource, XMLReader}

class Saxon(
  xslParameters: Map[String, String],
  entities: Map[String, String],
  xslDirectory: File,
  imagesDirectoryName: String,
  dataDirectory: File,
  evaluator: Evaluator,
  logger: Logger
) {
  logger.info(
    s"""Created new Saxon(
       |  xslParameters = $xslParameters,
       |  entities = $entities,
       |  xslDirectory = "$xslDirectory",
       |  imagesDirectoryName = "$imagesDirectoryName",
       |  dataDirectory = "$dataDirectory"
       |)""".stripMargin
  )

  logger.info("Configuring DocBookEntityResolver")
  val entityResolver: DocBookEntityResolver = new DocBookEntityResolver(
    entities = entities,
    dataDirectory = dataDirectory,
    logger = logger
  )

  logger.info("Configuring XMLReader")
  val saxParserFactory: SAXParserFactory = new SAXParserFactoryImpl
  saxParserFactory.setXIncludeAware(true)

  val xmlReader: XMLReader = new ProcessingInstructionsFilter(
    parent = saxParserFactory.newSAXParser.getXMLReader,
    evaluator = evaluator,
    logger = logger
  )
  xmlReader.setEntityResolver(entityResolver)

  logger.info("Configuring DocBookUriResolver")
  val uriResolver: DocBookUriResolver = new DocBookUriResolver(xslDirectory, logger)

  def resolve(uri: String): Source = uriResolver.resolve(new URI(uri))

  logger.info("Configuring TransformerFactory")
  val transformerFactory: TransformerFactory = new TransformerFactoryImpl
  // To intercept all network requests, URIResolver has to be set on the transformerFactory,
  // not the transformer itself: I guess some sub-transformers get created internally ;)
  transformerFactory.setURIResolver(uriResolver)


  def run(
    inputFile: File,
    stylesheetFile: File,
    outputFile: File
  ): Unit = run(
    inputSource = new InputSource(inputFile.toURI.toASCIIString),
    stylesheetSource = new StreamSource(stylesheetFile),
    outputTarget = new StreamResult(outputFile),
    outputFile = Some(outputFile)
  )

  def run(
    inputSource: InputSource,
    stylesheetSource: Source,
    outputTarget: Result,
    outputFile: Option[File]
  ): Unit = {
    s"""Saxon.run(
       |  inputSource = ${inputSource.getSystemId},
       |  stylesheetSource = ${stylesheetSource.getSystemId},
       |  outputFile = $outputFile
       |)""".stripMargin

    logger.info("Configuring Transformer")
    val transformer: Transformer = transformerFactory.newTransformer(stylesheetSource)

    logger.info("Setting Transformer parameters")
    setParameters(transformer, outputFile)

    outputFile.foreach(_.getParentFile.mkdirs)

    logger.info("Running transform")
    val xmlSource: SAXSource = new SAXSource(xmlReader, inputSource)
    transformer.transform(xmlSource, outputTarget)
  }

  private def setParameters(transformer: Transformer, outputFile: Option[File]): Unit = {
    def setParameter(name: String, value: String): Unit = {
      logger.info(s"Transformer parameter $name=$value")
      transformer.setParameter(name, value)
    }

    def setOptionalParameter(name: String, value: String): Unit =
      if (!xslParameters.contains(name)) setParameter(name, value)

    xslParameters.foreach { case (name: String, value: String) => setParameter(name, value) }

    // Relevant only for HTML and EPUB:
    setOptionalParameter("img.src.path", imagesDirectoryName + "/")
    setOptionalParameter("html.stylesheet", "css/docBook.css")

    // Relevant only for chunked HTML:
    outputFile.foreach { outputFile =>
      setOptionalParameter("root.filename", Util.fileNameWithoutExtension(outputFile))
      setOptionalParameter("base.dir", outputFile.getParent)
    }
  }
}

object Saxon {
  def apply(
    xslParameters: Map[String, String],
    entities: Map[String, String],
    xslDirectory: File,
    imagesDirectoryName: String,
    dataDirectory: File,
    evaluator: Evaluator,
    logger: Logger
  ): Saxon = new Saxon(
    xslParameters = xslParameters,
    entities = entities,
    xslDirectory = xslDirectory,
    imagesDirectoryName = imagesDirectoryName,
    dataDirectory = dataDirectory,
    evaluator = evaluator,
    logger = logger
  )
}
