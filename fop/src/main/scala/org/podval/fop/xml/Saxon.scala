package org.podval.fop.xml

import java.io.File

import javax.xml.transform.{Result, Source, Transformer}
import javax.xml.transform.sax.SAXTransformerFactory
import javax.xml.transform.stream.StreamSource
import org.podval.fop.util.Logger

sealed abstract class Saxon(name: String) {

  final override def toString: String = name

  def getTransformerFactory: SAXTransformerFactory = {
    val result: SAXTransformerFactory = newTransformerFactory

    // To process DocBook stylesheets (see also Svg.scala), Saxon needs real Xerces parser,
    // not the one included in the JDK or included with Saxon (com.icl.saxon.aelfred.SAXParserFactoryImpl).
    // Classpath-based discovery is unstable (order changes from one Gradle version to another) and ugly.
    // Tell Saxon to use Xerces parser explicitly:
    result.setAttribute(stylePraserClassAttribute, Xml.saxParserName)
    result.setAttribute(sourcePraserClassAttribute, Xml.saxParserName)

    result
  }

  protected def newTransformerFactory: SAXTransformerFactory

  protected def stylePraserClassAttribute: String

  protected def sourcePraserClassAttribute: String

  def transform(
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

    Xml.setErrorListener(transformerFactory, logger)

    val transformer: Transformer = stylesheetFile.fold(transformerFactory.newTransformer) {
      stylesheetFile => transformerFactory.newTransformer(new StreamSource(stylesheetFile))
    }

    transformer.transform(source, result)
  }
}

object Saxon {
  object Saxon6 extends Saxon("Saxon 6") {
    override protected def newTransformerFactory: SAXTransformerFactory = new com.icl.saxon.TransformerFactoryImpl
    override protected def stylePraserClassAttribute: String = com.icl.saxon.FeatureKeys.STYLE_PARSER_CLASS
    override protected def sourcePraserClassAttribute: String = com.icl.saxon.FeatureKeys.SOURCE_PARSER_CLASS
  }

  object Saxon9 extends Saxon("Saxon 9") {
    override protected def newTransformerFactory: SAXTransformerFactory = new net.sf.saxon.TransformerFactoryImpl
    override protected def stylePraserClassAttribute: String = net.sf.saxon.lib.FeatureKeys.STYLE_PARSER_CLASS
    override protected def sourcePraserClassAttribute: String = net.sf.saxon.lib.FeatureKeys.SOURCE_PARSER_CLASS
  }
}
