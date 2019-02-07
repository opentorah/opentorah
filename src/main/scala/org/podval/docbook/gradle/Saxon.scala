package org.podval.docbook.gradle

import java.io.File
import javax.xml.parsers.SAXParserFactory
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.{Result, Source, Transformer, TransformerFactory}
import org.xml.sax.{InputSource, XMLReader}

object Saxon {

  def run(
    inputSource: InputSource,
    stylesheetSource: Source,
    xslParameters: Map[String, String],
    entitySubstitutions: Map[String, String],
    processingInstructionsSubstitutions: Map[String, String],
    xslDirectory: File,
    dataDirectory: File,
    outputTarget: Result,
    logger: Logger
): Unit = {
    logger.info(
      s"""Saxon.run(
       |  inputSource = ${inputSource.getSystemId},
       |  stylesheetSource = ${stylesheetSource.getSystemId},
       |  xslParameters = $xslParameters,
       |  entitySubstitutions = $entitySubstitutions,
       |  processingInstructionsSubstitutions = $processingInstructionsSubstitutions,
       |  xslDirectory = "$xslDirectory",
       |  dataDirectory = "$dataDirectory",
       |  outputTarget = ${outputTarget.getSystemId}
       |)""".stripMargin
    )

    // To intercept all network requests, URIResolver has to be set on the transformerFactory,
    // not the transformer itself: I guess some sub-transformers get created internally ;)
    val transformerFactory: TransformerFactory = getTransformerFactory
    transformerFactory.setURIResolver(getUriResolver(
      xslDirectory = xslDirectory,
      logger = logger
    ))

    val transformer: Transformer = transformerFactory.newTransformer(stylesheetSource)

    xslParameters.foreach { case (name: String, value: String) => transformer.setParameter(name, value) }

    val saxParserFactory: SAXParserFactory = Saxon.getSaxParserFactory

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

  def getTransformerFactory: TransformerFactory =
    new com.icl.saxon.TransformerFactoryImpl

  def getSaxParserFactory: SAXParserFactory = {
    val result = new org.apache.xerces.jaxp.SAXParserFactoryImpl
    result.setXIncludeAware(true)
    result
  }

  def getUriResolver(
    xslDirectory: File,
    logger: Logger
  ): DocBookUriResolver = new DocBookUriResolver(xslDirectory, logger)
}
