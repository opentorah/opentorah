package org.podval.docbook.gradle.xml

import java.io.{File, FileWriter, StringReader}

import javax.xml.parsers.SAXParserFactory
import javax.xml.transform.dom.DOMResult
import javax.xml.transform.sax.{SAXResult, SAXSource, SAXTransformerFactory}
import javax.xml.transform.stream.{StreamResult, StreamSource}
import javax.xml.transform.{ErrorListener, Result, Source, Transformer, TransformerException}
import org.podval.fop.util.Logger
import org.podval.fop.xml.Xerces
import org.w3c.dom.Node
import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{ErrorHandler, InputSource, SAXParseException, XMLFilter, XMLReader}

object Xml {

  val header: String = """<?xml version="1.0" encoding="UTF-8"?>"""

  val header16: String = """<?xml version="1.0" encoding="UTF-16"?>"""

  def toString(node: Node): String = xmlSerializer.writeToString(node)

  private val xmlSerializer: org.apache.xml.serializer.dom3.LSSerializerImpl = {
    val result = new org.apache.xml.serializer.dom3.LSSerializerImpl
    result.setParameter("format-pretty-print", true)
    result
  }

  def getFilteredXMLReader(filters: Seq[XMLFilter]): XMLReader =
    filters.foldLeft(getXMLReader) { case (parent, filter) =>
        filter.setParent(parent)
        filter
    }

  private def getXMLReader: XMLReader =
    Xerces.newSaxParserFactory.newSAXParser.getXMLReader

  def transform(
    useSaxon9: Boolean,
    resolver: Resolver,
    inputFile: File,
    stylesheetFile: File,
    xmlReader: XMLReader,
    outputFile: Option[File],
    logger: Logger
  ): Unit = {
    xmlReader.setEntityResolver(resolver)

    setErrorHandler(xmlReader, logger)

    transform(
      useSaxon9,
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
    useSaxon9 = false,
    resolver = None,
    stylesheetFile = None,
    source = new StreamSource(inputFile),
    result = new SAXResult(defaultHandler),
    logger
  )

  def parse(input: String, xmlReader: XMLReader, logger: Logger): Node = {
    setErrorHandler(xmlReader, logger)

    val result = new DOMResult

    transform(
      useSaxon9 = true, // Saxon 6 returns unmodifiable DOM that breaks toString(); using Saxon 9.
      resolver = None,
      stylesheetFile = None,
      source = new SAXSource(xmlReader, new InputSource(new StringReader(input))),
      result = result,
      logger
    )

    result.getNode
  }

  private def transform(
    useSaxon9: Boolean,
    resolver: Option[Resolver],
    stylesheetFile: Option[File],
    source: Source,
    result: Result,
    logger: Logger
  ): Unit = {
    logger.debug(
      s"""Xml.transform(
         |  useSaxon9 = $useSaxon9,
         |  stylesheetFile = $stylesheetFile,
         |  source = ${source.getSystemId},
         |  result = ${result.getSystemId}
         |)""".stripMargin
    )

    val transformerFactory: SAXTransformerFactory =
      if (!useSaxon9) new com.icl.saxon.TransformerFactoryImpl else new net.sf.saxon.TransformerFactoryImpl

    // To process DocBook stylesheets (see also Svg.scala), Saxon needs real Xerces parser,
    // not the one included in the JDK or included with Saxon (com.icl.saxon.aelfred.SAXParserFactoryImpl).
    // Classpath-based discovery is unstable (order changes from one Gradle version to another) and ugly.
    // Tell Saxon to use Xerces parser explicitly:
    transformerFactory.setAttribute(
      if (!useSaxon9) com.icl.saxon.FeatureKeys.STYLE_PARSER_CLASS else net.sf.saxon.lib.FeatureKeys.STYLE_PARSER_CLASS,
      Xerces.saxParserName
    )
    transformerFactory.setAttribute(
      if (!useSaxon9) com.icl.saxon.FeatureKeys.SOURCE_PARSER_CLASS else net.sf.saxon.lib.FeatureKeys.SOURCE_PARSER_CLASS,
      Xerces.saxParserName
    )

    // Note: To intercept all network requests, URIResolver has to be set on the transformerFactory,
    // not the transformer itself: I guess some sub-transformers get created internally ;)
    resolver.foreach(resolver => transformerFactory.setURIResolver(resolver))

    setErrorListener(transformerFactory, logger)

    val transformer: Transformer = stylesheetFile.fold(transformerFactory.newTransformer) {
      stylesheetFile => transformerFactory.newTransformer(new StreamSource(stylesheetFile))
    }

    transformer.transform(source, result)
  }

  private def setErrorHandler(xmlReader: XMLReader, logger: Logger): Unit =
    xmlReader.setErrorHandler(new ErrorHandler {
      override def warning(exception: SAXParseException): Unit = logger.warn(exception.toString)
      override def error(exception: SAXParseException): Unit = logger.error(exception.toString)
      override def fatalError(exception: SAXParseException): Unit = logger.error(exception.toString)
    })

  private def setErrorListener(transformerFactory: SAXTransformerFactory, logger: Logger): Unit =
    transformerFactory.setErrorListener(new ErrorListener {
      override def warning(exception: TransformerException): Unit = logger.warn(exception.getMessageAndLocation)
      override def error(exception: TransformerException): Unit = logger.error(exception.getMessageAndLocation)
      override def fatalError(exception: TransformerException): Unit = logger.error(exception.getMessageAndLocation)
    })
}
