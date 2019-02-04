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
  entities: Map[String, String],
  substitutions: Map[String, String],
  xslDirectory: File,
  dataDirectory: File,
  logger: Logger
) {
  logger.info(
    s"""Created new Saxon(
       |  entities = $entities,
       |  substitutions = $substitutions,
       |  xslDirectory = "$xslDirectory",
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
    substitutions = entities ++ substitutions,
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
    xslParameters: Map[String, String],
    outputFile: File
  ): Unit = run(
    inputSource = new InputSource(inputFile.toURI.toASCIIString),
    stylesheetSource = new StreamSource(stylesheetFile),
    xslParameters = xslParameters,
    outputTarget = new StreamResult(outputFile)
  )

  def run(
    inputSource: InputSource,
    stylesheetSource: Source,
    xslParameters: Map[String, String],
    outputTarget: Result
  ): Unit = {
    s"""Saxon.run(
       |  inputSource = ${inputSource.getSystemId},
       |  stylesheetSource = ${stylesheetSource.getSystemId},
       |  xslParameters = $xslParameters,
       |  outputTarget = ${outputTarget.getSystemId}
       |)""".stripMargin

    logger.info("Configuring Transformer")
    val transformer: Transformer = transformerFactory.newTransformer(stylesheetSource)

    logger.info("Setting Transformer parameters")
    xslParameters.foreach { case (name: String, value: String) => transformer.setParameter(name, value) }

    logger.info("Running transform")
    val xmlSource: SAXSource = new SAXSource(xmlReader, inputSource)
    transformer.transform(xmlSource, outputTarget)
  }
}
