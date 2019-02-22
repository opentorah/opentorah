package org.podval.docbook.gradle

import javax.xml.parsers.SAXParserFactory
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.{Result, Source, Transformer, TransformerFactory}
import org.xml.sax.{InputSource, XMLReader}

object Saxon {

  def run(
    inputSource: InputSource,
    stylesheetSource: Source,
    resolver: Resolver,
    processingInstructionsSubstitutions: Map[String, String],
    outputTarget: Result,
    useXslt2: Boolean,
    logger: Logger
  ): Unit = {
    logger.info(
      s"""Saxon.run(
       |  inputSource = ${inputSource.getSystemId},
       |  stylesheetSource = ${stylesheetSource.getSystemId},
       |  processingInstructionsSubstitutions = $processingInstructionsSubstitutions,
       |  outputTarget = ${outputTarget.getSystemId},
       |  useXslt2 = $useXslt2
       |)""".stripMargin
    )

    val transformerFactory: TransformerFactory =
      if (useXslt2) getXslt2TransformerFactory else getXslt1TransformerFactory

    // To intercept all network requests, URIResolver has to be set on the transformerFactory,
    // not the transformer itself: I guess some sub-transformers get created internally ;)
    transformerFactory.setURIResolver(resolver)

    val transformer: Transformer = transformerFactory.newTransformer(stylesheetSource)
    transformer.setErrorListener(logger.errorListener)

    val saxParserFactory: SAXParserFactory = Saxon.getSaxParserFactory

    val xmlReader: XMLReader = new ProcessingInstructionsFilter(
      parent = saxParserFactory.newSAXParser.getXMLReader,
      substitutions = processingInstructionsSubstitutions,
      logger = logger
    )
    xmlReader.setEntityResolver(resolver)

    val xmlSource: SAXSource = new SAXSource(xmlReader, inputSource)
    transformer.transform(xmlSource, outputTarget)
  }

  def getXslt1TransformerFactory: TransformerFactory = new com.icl.saxon.TransformerFactoryImpl

  def getXslt2TransformerFactory: TransformerFactory = new net.sf.saxon.TransformerFactoryImpl

  def getSaxParserFactory: SAXParserFactory = {
    val result = new org.apache.xerces.jaxp.SAXParserFactoryImpl
    result.setXIncludeAware(true)
    result
  }
}
