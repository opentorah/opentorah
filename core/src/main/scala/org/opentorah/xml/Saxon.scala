package org.opentorah.xml

import org.slf4j.Logger
import org.xml.sax.{InputSource, XMLFilter}
import java.io.{File, FileWriter}
import javax.xml.transform.{ErrorListener, Transformer, TransformerException}
import javax.xml.transform.sax.{SAXSource, SAXTransformerFactory}
import javax.xml.transform.stream.StreamResult

// Note: DocBook XSLT uses Saxon 6 XSLT 1.0 extensions and doesn't work on later Saxon versions
// ("Don't know how to chunk with Saxonica").
// According to https://www.saxonica.com/html/documentation/extensions/instructions/output.html,
//   "Saxon 9.9 reintroduces saxon6:output (in the original Saxon 6.5.5 namespace,
//   which differs from the usual Saxon namespace, so here we use a different prefix)
//   so that the DocBook 1.0 stylesheets can now be executed with a modern Saxon release.
//   Note that the specification is not identical with the Saxon 6.5.5 original,
//   but it serves the purpose in supporting DocBook."
// I am not sure what I can do to set up DocBook XSLT 1 processing with Saxon 10 // TODO try 11!
// (it didn't work out of the box for me), but I'd love to get rid of the Saxon 6, since it:
// - produces unmodifiable DOM - unlike Saxon 10+,
// - carries within it obsolete org.w3c.dom classes (Level 2), which cause IDE to highlight
//   as errors uses of the (Level 3) method org.w3c.dom.Node.getTextContent()...
sealed abstract class Saxon(name: String):

  final override def toString: String = name

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
    val result: SAXTransformerFactory = newTransformerFactory

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
    result.setAttribute(styleParserClassAttribute, Xerces.saxParserName)
    result.setAttribute(sourceParserClassAttribute, Xerces.saxParserName)

    result

  protected def newTransformerFactory: SAXTransformerFactory

  protected def styleParserClassAttribute: String

  protected def sourceParserClassAttribute: String

object Saxon:
  object Saxon6 extends Saxon("Saxon 6"):
    override protected def newTransformerFactory: SAXTransformerFactory = com.icl.saxon.TransformerFactoryImpl()
    override protected def styleParserClassAttribute: String = com.icl.saxon.FeatureKeys.STYLE_PARSER_CLASS
    override protected def sourceParserClassAttribute: String = com.icl.saxon.FeatureKeys.SOURCE_PARSER_CLASS

  object Saxon11 extends Saxon("Saxon 11"):
    override protected def newTransformerFactory: SAXTransformerFactory = net.sf.saxon.TransformerFactoryImpl()
    override protected def styleParserClassAttribute: String = net.sf.saxon.lib.FeatureKeys.STYLE_PARSER_CLASS
    override protected def sourceParserClassAttribute: String = net.sf.saxon.lib.FeatureKeys.SOURCE_PARSER_CLASS

  private def errorListener(logger: Logger): ErrorListener = new ErrorListener:
    override def warning   (exception: TransformerException): Unit = logger.warn (message(exception))
    override def error     (exception: TransformerException): Unit = logger.error(message(exception))
    override def fatalError(exception: TransformerException): Unit = logger.error(message(exception))

    private def message(exception: TransformerException): String = exception.getMessageAndLocation

  // do not output the 'main' file when chunking in XSLT 1.0
  def result(usesRootFile: Boolean, outputFile: File): StreamResult =
    val result = new StreamResult
    if usesRootFile then
      result.setSystemId(outputFile)
      result.setWriter(FileWriter(outputFile))
    else
      result.setSystemId("dev-null")
      result.setOutputStream((_: Int) => {})
    result
