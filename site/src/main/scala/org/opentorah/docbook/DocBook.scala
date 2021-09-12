package org.opentorah.docbook

import org.opentorah.html
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Catalog, Dialect, Doctype, Dom, Namespace, PrettyPrinter, Resolver, ScalaXml}
import zio.{Has, URIO}
import java.io.File
import java.net.URI

object DocBook extends Dialect, Doctype, html.ToHtml[Has[Unit]]:

  override val namespace: Namespace = Namespace(uri = "http://docbook.org/ns/docbook", prefix = "db")

  override val mimeType: String = "application/xml"

  val dtdId: String = "-//OASIS//DTD DocBook XML V5.0//EN"

  val dtdUri: String = "https://docbook.org/xml/5.0/dtd/docbook.dtd"
  // was: "http://www.oasis-open.org/docbook/xml/5.0/dtd/docbook.dtd"

  override val doctype: String = doctypeString("article")

  def doctypeString(rootElementName: String): String = s"""<!DOCTYPE $rootElementName PUBLIC "$dtdId" "$dtdUri">"""

  def doctype(rootElementName: String): Doctype = new Doctype:
    override def doctype: String = doctypeString(rootElementName)

  val version: String = "5.0"

  def data(dataDirectory: File): Seq[ScalaXml.Element] = Seq(
    "data:",
    "data:/",
    "urn:docbook:data:/",
    "urn:docbook:data:",
    "urn:docbook:data/",
    "http://opentorah.org/docbook/data/"
  ).map(dataSystemId => Catalog.rewriteSystem(
    rewritePrefix = Files.file2url(dataDirectory).toString,
    systemIdStartString = dataSystemId
  ))

  def dtdLink(dtdFile: File): ScalaXml.Element =
    Catalog.public(publicId = DocBook.dtdId, uri = Files.file2url(dtdFile).toString)

  def writeDtd(
    dtdFile: File,
    substitutions: Map[String, String]
  ): Unit = Files.write(
    file = dtdFile,
    replace = true,
    content = Catalog.dtd(substitutions)
  )

  override val prettyPrinter: PrettyPrinter = PrettyPrinter(
    alwaysStackElements = Set("book", "part", "article", "para", //"itemizedlist",
      "equation", "informalequation", "inlineequation", "math", "mrow", "mi"),
    clingyElements = Set("footnote")
  )

  // TODO Scala XML does not work with XInclude-aware parsers
  // (see https://github.com/scala/scala-xml/issues/506),
  // but DocBook uses XInclude to assemble the document,
  // so I parse to Dom, pretty-print combined document to String and re-parse it with Xml:
  def loadFromFile(file: File, resolver: Option[Resolver]): ScalaXml.Element =
    val element: Dom.Element = Dom.loadFromUrl(Files.file2url(file), resolver = resolver)
    val string: String = DocBook.prettyPrinter.renderWithHeader(Dom)(element)
    ScalaXml.loadFromString(string)

  // TODO put endnotes below the component that has them
  override protected def isEndNote(element: ScalaXml.Element): Boolean = ScalaXml.getName(element) == "footnote"

  private val urlAttribute: Attribute.Required[String] = Attribute("url").required
  private val linkendAttribute: Attribute.Required[String] = Attribute("linkend").required

  override protected def elementTransform(element: ScalaXml.Element): URIO[Has[Unit], ScalaXml.Element] =
    val children: ScalaXml.Nodes = ScalaXml.getChildren(element)

    ScalaXml.getName(element) match
      // TODO link, olink, xref, anchor

      case "ulink" =>
        require(!ScalaXml.isEmpty(children), element)
        URIO.succeed(html.a(URI(urlAttribute.get(ScalaXml)(element)))(children))

      // TODO look up the reference in the bibliography entry itself
      // TODO add chunk name for chunked mode
      case "biblioref" =>
        require(ScalaXml.isEmpty(children), element)
        val linkend: String = linkendAttribute.get(ScalaXml)(element)
        URIO.succeed(html.a().setFragment(linkend)(linkend))

      //      case "graphic" =>
      //        // Note: in TEI <graphic> can contain <desc>, but we are treating it as empty.
      //        require(Xml.isEmpty(children), element)
      //        URIO.succeed(<img src={urlAttribute.get(element)}/>)

      // TODO tgroups etc.; colspan analogue?
      case "informaltable" =>
        URIO.succeed(<table>{children}</table>)

      case "row" =>
        URIO.succeed(<tr>{children}</tr>)

      case "entry" =>
        //URIO.succeed(<td colspan={colsAttribute.get(element).orNull}>{children}</td>)
        URIO.succeed(<td>{children}</td>)

      case _ =>
        URIO.succeed(element)
