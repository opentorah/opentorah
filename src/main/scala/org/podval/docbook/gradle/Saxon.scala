package org.podval.docbook.gradle

import com.icl.saxon.TransformerFactoryImpl
import java.io.File
import java.net.URI
import javax.xml.parsers.SAXParserFactory
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.{Result, Source, Transformer, TransformerFactory}
import org.apache.xerces.jaxp.SAXParserFactoryImpl
import org.xml.sax.{InputSource, XMLReader}

class Saxon(
  xslDirectory: File,
  dataDirectory: File,
  logger: Logger
) {
  logger.info(
    s"""Created new Saxon(
       |  xslDirectory = "$xslDirectory",
       |  dataDirectory = "$dataDirectory"
       |)""".stripMargin
  )

  private val uriResolver: DocBookUriResolver = new DocBookUriResolver(xslDirectory, logger)

  def resolve(uri: String): Source = uriResolver.resolve(new URI(uri))

  private val transformerFactory: TransformerFactory = new TransformerFactoryImpl
  // To intercept all network requests, URIResolver has to be set on the transformerFactory,
  // not the transformer itself: I guess some sub-transformers get created internally ;)
  transformerFactory.setURIResolver(uriResolver)

  private val saxParserFactory: SAXParserFactory = new SAXParserFactoryImpl
  saxParserFactory.setXIncludeAware(true)

  private def getEntityResolver(substitutions: Map[String, String]): DocBookEntityResolver = new DocBookEntityResolver(
    entities = substitutions,
    dataDirectory = dataDirectory,
    logger = logger
  )

  private def getXmlReader(
    entitySubstitutions: Map[String, String],
    processingInstructionsSubstitutions: Map[String, String]
  ): XMLReader = {
    val result: XMLReader = new ProcessingInstructionsFilter(
      parent = saxParserFactory.newSAXParser.getXMLReader,
      substitutions = processingInstructionsSubstitutions,
      logger = logger
    )
    result.setEntityResolver(getEntityResolver(entitySubstitutions))
    result
  }

  def run(
    inputSource: InputSource,
    stylesheetSource: Source,
    xslParameters: Map[String, String],
    entitySubstitutions: Map[String, String],
    processingInstructionsSubstitutions: Map[String, String],
    outputTarget: Result
  ): Unit = {
    s"""Saxon.run(
       |  inputSource = ${inputSource.getSystemId},
       |  stylesheetSource = ${stylesheetSource.getSystemId},
       |  xslParameters = $xslParameters,
       |  entitySubstitutions = $entitySubstitutions,
       |  processingInstructionsSubstitutions = $processingInstructionsSubstitutions,
       |  outputTarget = ${outputTarget.getSystemId}
       |)""".stripMargin

    logger.info("Configuring Transformer")
    val transformer: Transformer = transformerFactory.newTransformer(stylesheetSource)

    logger.info("Setting Transformer parameters")
    xslParameters.foreach { case (name: String, value: String) => transformer.setParameter(name, value) }

    logger.info("Running transform")
    val xmlReader: XMLReader = getXmlReader(
      entitySubstitutions = entitySubstitutions,
      processingInstructionsSubstitutions = processingInstructionsSubstitutions
    )
    val xmlSource: SAXSource = new SAXSource(xmlReader, inputSource)
    transformer.transform(xmlSource, outputTarget)
  }
}
