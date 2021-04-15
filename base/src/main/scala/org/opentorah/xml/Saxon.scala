package org.opentorah.xml

import org.slf4j.Logger
import java.io.File
import javax.xml.transform.sax.{SAXSource, SAXTransformerFactory}
import javax.xml.transform.{ErrorListener, Transformer, TransformerException}
import org.xml.sax.{InputSource, XMLFilter}

sealed abstract class Saxon(name: String) {

  final override def toString: String = name

  def transform(
    filters: Seq[XMLFilter],
    resolver: Option[Resolver],
    stylesheetFile: Option[File],
    inputSource: InputSource,
    result: javax.xml.transform.Result
  ): Unit = {
    xmlLogger.debug(
      s"""Saxon.transform(
         |  saxon = $this,
         |  stylesheetFile = $stylesheetFile,
         |  source = ${inputSource.getSystemId},
         |  result = ${result.getSystemId}
         |)""".stripMargin
    )

    val transformerFactory: SAXTransformerFactory = getTransformerFactory(resolver)

    val transformer: Transformer = stylesheetFile.fold(transformerFactory.newTransformer) {
      stylesheetFile => transformerFactory.newTransformer(new SAXSource(
        Xerces.getXMLReader(filters = Seq.empty, resolver, xincludeAware = true), // no need to filter stylesheets
        Sax.file2inputSource(stylesheetFile)
      ))
    }

    transformer.transform(
      new SAXSource(
        Xerces.getXMLReader(filters, resolver, xincludeAware = true),
        inputSource
      ),
      result
    )
  }

  private def getTransformerFactory(resolver: Option[Resolver]): SAXTransformerFactory = {
    val result: SAXTransformerFactory = newTransformerFactory

    result.setErrorListener(Saxon.errorListener(xmlLogger))

    // Note: To intercept all network requests, URIResolver has to be set on the transformerFactory,
    // not the transformer itself: I guess some sub-transformers get created internally ;)
    // TODO now that XMLReader is supplied explicitly, maybe this is not needed?
    resolver.foreach(resolver => result.setURIResolver(resolver))

    // To process DocBook stylesheets (see also Svg.scala), Saxon needs real Xerces parser,
    // not the one included in the JDK or included with Saxon (com.icl.saxon.aelfred.SAXParserFactoryImpl).
    // Classpath-based discovery is unstable (order changes from one Gradle version to another) and ugly.
    // Tell Saxon to use Xerces parser explicitly:
    // TODO now that XMLReader is supplied explicitly, maybe this is not needed?
    result.setAttribute(styleParserClassAttribute, Xerces.saxParserName)
    result.setAttribute(sourceParserClassAttribute, Xerces.saxParserName)

    result
  }

  protected def newTransformerFactory: SAXTransformerFactory

  protected def styleParserClassAttribute: String

  protected def sourceParserClassAttribute: String
}

object Saxon {

  object Saxon6 extends Saxon("Saxon 6") {
    override protected def newTransformerFactory: SAXTransformerFactory = new com.icl.saxon.TransformerFactoryImpl
    override protected def styleParserClassAttribute: String = com.icl.saxon.FeatureKeys.STYLE_PARSER_CLASS
    override protected def sourceParserClassAttribute: String = com.icl.saxon.FeatureKeys.SOURCE_PARSER_CLASS
  }

  // Saxon6 produces unmodifiable DOM, which can not be serialized; Saxon 10's DOM can.
  object Saxon10 extends Saxon("Saxon 10") {
    override protected def newTransformerFactory: SAXTransformerFactory = new net.sf.saxon.TransformerFactoryImpl
    override protected def styleParserClassAttribute: String = net.sf.saxon.lib.FeatureKeys.STYLE_PARSER_CLASS
    override protected def sourceParserClassAttribute: String = net.sf.saxon.lib.FeatureKeys.SOURCE_PARSER_CLASS
  }

  def errorListener(logger: Logger): ErrorListener = new ErrorListener {
    override def warning   (exception: TransformerException): Unit = logger.warn (message(exception))
    override def error     (exception: TransformerException): Unit = logger.error(message(exception))
    override def fatalError(exception: TransformerException): Unit = logger.error(message(exception))

    private def message(exception: TransformerException): String = exception.getMessageAndLocation
  }
}
