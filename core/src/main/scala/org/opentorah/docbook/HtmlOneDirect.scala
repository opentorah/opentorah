package org.opentorah.docbook

import org.opentorah.html.{SiteHtml, Html as XHtml}
import org.opentorah.math.MathConfiguration
import org.opentorah.util.Files
import org.opentorah.xml.{Dom, Parser, PrettyPrinter, Resolver, ScalaXml}
import java.io.File
import zio.ZLayer

object HtmlOneDirect extends DirectFormat:
  override def name: String = "htmlOneDirect"
  override def outputFileExtension: String = "html"
  override protected def outputFileNameOverride: Option[String] = Some("index")
  override def common: List[Common] = List(AllCommon, HtmlCommon)
  override def parameters: Map[String, String] = Map.empty

  final override def process(
    resolver: Resolver,
    inputFile: File,
    parameters: Parameters,
    math: MathConfiguration,
    siteHtml: SiteHtml,
    processOutputFile: File
  ): Unit =
    // TODO Scala XML does not work with XInclude-aware parsers
    // (see https://github.com/scala/scala-xml/issues/506),
    // but DocBook uses XInclude to assemble the document,
    // so I parse to Dom, pretty-print combined document to String and re-parse it with ScalaXml:
    val dom: Dom.Element = Dom.loadFromUrl(Files.file2url(inputFile), resolver = Some(resolver))
    val xml: ScalaXml.Element = ScalaXml.loadFromString(DocBook.prettyPrinter.renderWithHeader(Dom)(dom))

  // TODO extract from DocBook:
    val headTitle: Option[String] = None
    val bodyTitle: Option[ScalaXml.Nodes] = None

    // TODO factor out commonality with Site.render() and Site.resolveLinks()

    val fullContent: ScalaXml.Element = XHtml.fullContent(
      wrapperCssClass = null,
      header = None,
      bodyTitle = bodyTitle,
      content = xml
    )

    val result: Parser[ScalaXml.Element] = for
      content: ScalaXml.Element <- DocBook.toHtml(fullContent).provideLayer(ZLayer.succeed(()))
    yield XHtml.toHtml(
      siteHtml = siteHtml,
      math = math,
      headTitle = headTitle,
      cssFileName = XHtml.styleDefault,
      viewer = XHtml.viewerDefault,
      navigationLinks = Seq.empty, // TODO move pages into SiteHtml?
      content = content
    )

    Files.write(
      processOutputFile,
      content = DirectFormat.prettyPrinter.render(ScalaXml, doctype = Some(XHtml))(Parser.unsafeRun(result))
    )
