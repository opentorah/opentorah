package org.opentorah.xml

import org.slf4j.Logger
import org.xml.sax.{InputSource, XMLFilter}
import java.io.File
import javax.xml.transform.{ErrorListener, Transformer, TransformerException}
import javax.xml.transform.sax.{SAXSource, SAXTransformerFactory}

object Saxon:
  final override def toString: String = "Saxon 11"

  def transform(
    filters: Seq[XMLFilter],
    resolver: Option[Resolver],
    stylesheetFile: Option[File],
    inputSource: InputSource,
    result: javax.xml.transform.Result,
    logger: Logger
  ): Unit =
    logger.debug(
      s"""Saxon.transform(
         |  saxon = $this,
         |  stylesheetFile = $stylesheetFile,
         |  source = ${inputSource.getSystemId},
         |  result = ${result.getSystemId}
         |)""".stripMargin
    )

    val transformerFactory: SAXTransformerFactory = getTransformerFactory(resolver, logger)

    val transformer: Transformer = stylesheetFile.fold(transformerFactory.newTransformer)(
      stylesheetFile => transformerFactory.newTransformer(SAXSource(
        Xerces.getXMLReader(
          filters = Seq.empty, // no need to filter stylesheets
          resolver = resolver,
          processIncludes = Xerces.ProcessIncludes.YesWithoutBases,
          logger = logger
        ),
        Sax.file2inputSource(stylesheetFile)
      ))
    )

    transformer.transform(
      SAXSource(
        Xerces.getXMLReader(
          filters = filters,
          resolver = resolver,
          processIncludes = Xerces.ProcessIncludes.YesWithBases,
          logger = logger
        ),
        inputSource
      ),
      result
    )

  private def getTransformerFactory(resolver: Option[Resolver], logger: Logger): SAXTransformerFactory =
    val result: SAXTransformerFactory = net.sf.saxon.TransformerFactoryImpl()

    result.setErrorListener(Saxon.errorListener(logger))

    // Note: To intercept all network requests, URIResolver has to be set on the transformerFactory,
    // not the transformer itself: I guess some sub-transformers get created internally ;)
    // TODO now that XMLReader is supplied explicitly, maybe this is not needed?
    resolver.foreach(resolver => result.setURIResolver(resolver))

    // To process DocBook stylesheets (see also Svg.scala), Saxon needs real Xerces parser,
    // not the one included in the JDK or included with Saxon (com.icl.saxon.aelfred.SAXParserFactoryImpl).
    // Classpath-based discovery is unstable (order changes from one Gradle version to another) and ugly.
    // Tell Saxon to use Xerces parser explicitly:
    // TODO now that XMLReader is supplied explicitly, maybe this is not needed?
    result.setAttribute(net.sf.saxon.lib.FeatureKeys.STYLE_PARSER_CLASS, Xerces.saxParserName)
    result.setAttribute(net.sf.saxon.lib.FeatureKeys.SOURCE_PARSER_CLASS, Xerces.saxParserName)

    result

  private def errorListener(logger: Logger): ErrorListener = new ErrorListener:
    private def message(exception: TransformerException): String = exception.getMessageAndLocation

    override def warning   (exception: TransformerException): Unit = logger.warn (message(exception))
    override def error     (exception: TransformerException): Unit = logger.error(message(exception))
    override def fatalError(exception: TransformerException): Unit = logger.error(message(exception))
