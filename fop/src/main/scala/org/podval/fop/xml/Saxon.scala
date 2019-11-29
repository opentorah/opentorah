package org.podval.fop.xml

import java.io.{File, FileWriter, StringReader}

import javax.xml.transform.dom.DOMResult
import javax.xml.transform.{ErrorListener, Result, Source, Transformer, TransformerException}
import javax.xml.transform.sax.{SAXResult, SAXSource, SAXTransformerFactory}
import javax.xml.transform.stream.{StreamResult, StreamSource}
import org.podval.fop.util.Logger
import org.w3c.dom.Node
import org.xml.sax.{ErrorHandler, InputSource, SAXParseException, XMLReader}
import org.xml.sax.helpers.DefaultHandler

sealed abstract class Saxon(name: String) {

  final override def toString: String = name

  protected final def getTransformerFactory: SAXTransformerFactory = {
    val result: SAXTransformerFactory = newTransformerFactory

    // To process DocBook stylesheets (see also Svg.scala), Saxon needs real Xerces parser,
    // not the one included in the JDK or included with Saxon (com.icl.saxon.aelfred.SAXParserFactoryImpl).
    // Classpath-based discovery is unstable (order changes from one Gradle version to another) and ugly.
    // Tell Saxon to use Xerces parser explicitly:
    result.setAttribute(styleParserClassAttribute, Xml.saxParserName)
    result.setAttribute(sourceParserClassAttribute, Xml.saxParserName)

    result
  }

  protected def newTransformerFactory: SAXTransformerFactory

  protected def styleParserClassAttribute: String

  protected def sourceParserClassAttribute: String

  def transform(
    resolver: Resolver,
    inputFile: File,
    stylesheetFile: File,
    xmlReader: XMLReader,
    outputFile: Option[File],
    logger: Logger
  ): Unit = {
    xmlReader.setEntityResolver(resolver)

    Saxon.setErrorHandler(xmlReader, logger)

    transform(
      resolver = Some(resolver),
      stylesheetFile = Some(stylesheetFile),
      source = new SAXSource(xmlReader, new InputSource(inputFile.toURI.toASCIIString)),
      result = getOutputTarget(outputFile),
      logger
    )
  }

  private def getOutputTarget(outputFile: Option[File]): Result = {
    val result = new StreamResult
    outputFile.map { outputFile =>
      result.setSystemId(outputFile)
      result.setWriter(new FileWriter(outputFile))
      result
    }.getOrElse {
      result.setSystemId("dev-null")
      result.setOutputStream((_: Int) => {})
      result
    }
  }

  def transform(
    inputFile: File,
    defaultHandler: DefaultHandler,
    logger: Logger
  ): Unit = transform(
    resolver = None,
    stylesheetFile = None,
    source = new StreamSource(inputFile),
    result = new SAXResult(defaultHandler),
    logger
  )

  // Saxon 6 returns unmodifiable DOM, Saxon 9 - modifiable.
  def parse(
    input: String,
    xmlReader: XMLReader,
    logger: Logger
  ): Node = {
    Saxon.setErrorHandler(xmlReader, logger)

    val result = new DOMResult

    transform(
      resolver = None,
      stylesheetFile = None,
      source = new SAXSource(xmlReader, new InputSource(new StringReader(input))),
      result = result,
      logger
    )

    result.getNode
  }

  private def transform(
    resolver: Option[Resolver],
    stylesheetFile: Option[File],
    source: Source,
    result: Result,
    logger: Logger
  ): Unit = {
    logger.debug(
      s"""Saxon.transform(
         |  saxon = $this,
         |  stylesheetFile = $stylesheetFile,
         |  source = ${source.getSystemId},
         |  result = ${result.getSystemId}
         |)""".stripMargin
    )

    val transformerFactory: SAXTransformerFactory = getTransformerFactory

    // Note: To intercept all network requests, URIResolver has to be set on the transformerFactory,
    // not the transformer itself: I guess some sub-transformers get created internally ;)
    resolver.foreach(resolver => transformerFactory.setURIResolver(resolver))

    Saxon.setErrorListener(transformerFactory, logger)

    val transformer: Transformer = stylesheetFile.fold(transformerFactory.newTransformer) {
      stylesheetFile => transformerFactory.newTransformer(new StreamSource(stylesheetFile))
    }

    transformer.transform(source, result)
  }
}

object Saxon {
  // Only Saxon6 is capable of handling DocBook XSLT stylesheets with their XSLT 1.0 extensions;
  // Saxon9 is not compatin=ble with that,
  object Saxon6 extends Saxon("Saxon 6") {
    override protected def newTransformerFactory: SAXTransformerFactory = new com.icl.saxon.TransformerFactoryImpl
    override protected def styleParserClassAttribute: String = com.icl.saxon.FeatureKeys.STYLE_PARSER_CLASS
    override protected def sourceParserClassAttribute: String = com.icl.saxon.FeatureKeys.SOURCE_PARSER_CLASS
  }

  // Saxon6 produces unmodifiable DOM, which can not be serialized; Saxon9's DOM can.
  object Saxon9 extends Saxon("Saxon 9") {
    override protected def newTransformerFactory: SAXTransformerFactory = new net.sf.saxon.TransformerFactoryImpl
    override protected def styleParserClassAttribute: String = net.sf.saxon.lib.FeatureKeys.STYLE_PARSER_CLASS
    override protected def sourceParserClassAttribute: String = net.sf.saxon.lib.FeatureKeys.SOURCE_PARSER_CLASS
  }

  def setErrorHandler(xmlReader: XMLReader, logger: Logger): Unit =
    xmlReader.setErrorHandler(new ErrorHandler {
      override def warning(exception: SAXParseException): Unit = logger.warn(exception.toString)
      override def error(exception: SAXParseException): Unit = logger.error(exception.toString)
      override def fatalError(exception: SAXParseException): Unit = logger.error(exception.toString)
    })

  def setErrorListener(transformerFactory: SAXTransformerFactory, logger: Logger): Unit =
    transformerFactory.setErrorListener(new ErrorListener {
      override def warning(exception: TransformerException): Unit = logger.warn(exception.getMessageAndLocation)
      override def error(exception: TransformerException): Unit = logger.error(exception.getMessageAndLocation)
      override def fatalError(exception: TransformerException): Unit = logger.error(exception.getMessageAndLocation)
    })
}
