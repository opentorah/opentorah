package org.podval.docbook.gradle

import java.io.File
import java.net.URI
import javax.xml.parsers.SAXParserFactory
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.{Result, Source, Transformer, TransformerFactory}
import org.xml.sax.{InputSource, XMLReader}

class Saxon(
  xslDirectory: File,
  logger: Logger
) {
  logger.info(
    s"""Created new Saxon(
       |  xslDirectory = "$xslDirectory",
       |)""".stripMargin
  )

  private val uriResolver: DocBookUriResolver = new DocBookUriResolver(xslDirectory, logger)

  def resolve(uri: String): Source = uriResolver.resolve(new URI(uri))

  // To intercept all network requests, URIResolver has to be set on the transformerFactory,
  // not the transformer itself: I guess some sub-transformers get created internally ;)
  private val transformerFactory: TransformerFactory = Saxon.getTransformerFactory
  transformerFactory.setURIResolver(uriResolver)

  private val saxParserFactory: SAXParserFactory = Saxon.getSaxParserFactory

  def run(
    inputSource: InputSource,
    stylesheetSource: Source,
    xslParameters: Map[String, String],
    entitySubstitutions: Map[String, String],
    processingInstructionsSubstitutions: Map[String, String],
    dataDirectory: File,
    outputTarget: Result
  ): Unit = {
    s"""Saxon.run(
       |  inputSource = ${inputSource.getSystemId},
       |  stylesheetSource = ${stylesheetSource.getSystemId},
       |  xslParameters = $xslParameters,
       |  entitySubstitutions = $entitySubstitutions,
       |  processingInstructionsSubstitutions = $processingInstructionsSubstitutions,
       |  dataDirectory = "$dataDirectory",
       |  outputTarget = ${outputTarget.getSystemId}
       |)""".stripMargin

    val transformer: Transformer = transformerFactory.newTransformer(stylesheetSource)

    xslParameters.foreach { case (name: String, value: String) => transformer.setParameter(name, value) }

    val xmlReader: XMLReader = new ProcessingInstructionsFilter(
      parent = saxParserFactory.newSAXParser.getXMLReader,
      substitutions = processingInstructionsSubstitutions,
      logger = logger
    )
    xmlReader.setEntityResolver(new DocBookEntityResolver(
      entities = entitySubstitutions,
      dataDirectory = dataDirectory,
      logger = logger
    ))
    val xmlSource: SAXSource = new SAXSource(xmlReader, inputSource)
    transformer.transform(xmlSource, outputTarget)
  }
}

object Saxon {
  def getTransformerFactory: TransformerFactory =
    new com.icl.saxon.TransformerFactoryImpl

  def getSaxParserFactory: SAXParserFactory = {
    val result = new org.apache.xerces.jaxp.SAXParserFactoryImpl
    result.setXIncludeAware(true)
    result
  }
}
