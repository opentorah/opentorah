package org.opentorah.docbook

import org.opentorah.html
import org.opentorah.metadata.Names
import org.opentorah.store.{Context, Path, Viewer}
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Catalog, Dialect, Doctype, Dom, Namespace, Parser, PrettyPrinter, Resolver, ScalaXml}
import zio.{Has, URIO}
import java.io.File
import java.net.URI
import zio.ZIO

// TODO dissolve into Site: introduce [Pre]Content and subsume this and Markdown into it.
final class DocBook(
  inputFile: File,
  resolver: Resolver
) extends Viewer.Default:
  override def names: Names = ??? // TODO
  override def htmlHeadTitle: Option[String] = None // TODO
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = None // TODO

  // TODO Caching.Parser?
  override def content(path: Path, context: Context): Parser[ScalaXml.Element] =
    ZIO.succeed(DocBook.loadFromFile(inputFile, Some(resolver))) // TODO real Parser

object DocBook extends Dialect, Doctype, html.ToHtml[Has[Unit]]:

  override val namespace: Namespace = Namespace(uri = "http://docbook.org/ns/docbook", prefix = "db")

  override val mimeType: String = "application/xml"

  val dtdId: String = "-//OASIS//DTD DocBook XML V5.0//EN"

  override val doctype: String = doctypeString("article")

  def doctype(rootElementName: String): Doctype = new Doctype:
    override def doctype: String = doctypeString(rootElementName)
   
  private def doctypeString(rootElementName: String): String = Doctype.string(
    rootElementName,
    dtdId,
    dtdUri = "https://docbook.org/xml/5.0/dtd/docbook.dtd"
    // was: "http://www.oasis-open.org/docbook/xml/5.0/dtd/docbook.dtd"
  )

  val version: String = "5.0"

  def data(dataDirectory: File): Seq[ScalaXml.Element] =
    for dataSystemId <- Seq(
    "data:",
    "data:/",
    "urn:docbook:data:/",
    "urn:docbook:data:",
    "urn:docbook:data/",
    "http://opentorah.org/docbook/data/"
  ) yield Catalog.rewriteSystem(
    rewritePrefix = Files.file2url(dataDirectory).toString,
    systemIdStartString = dataSystemId
  )

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

  override protected def isFootnote(element: ScalaXml.Element): Boolean = ScalaXml.getName(element) == "footnote"

  private val footnotesContainers: Set[String] = Set("chapter")

  override protected def isFootnotesContainer(element: ScalaXml.Element): Boolean =
    footnotesContainers.contains(ScalaXml.getName(element))

  private val urlAttribute: Attribute.Required[String] = Attribute("url").required
  private val linkendAttribute: Attribute.Required[String] = Attribute("linkend").required

  override protected def elementToHtml(
    element: ScalaXml.Element
  ): URIO[Has[Unit], (ScalaXml.Element, ScalaXml.Nodes)] =
    val name: String = ScalaXml.getName(element)
    val children: ScalaXml.Nodes = ScalaXml.getChildren(element)

    name match
      // TODO link, olink, xref, anchor

      case "ulink" =>
        require(!ScalaXml.isEmpty(children), element)
        succeed(html.a(URI(urlAttribute.get(ScalaXml)(element)))(children))

      // TODO look up the reference in the bibliography entry itself
      // TODO add chunk name for chunked mode
      case "biblioref" =>
        require(ScalaXml.isEmpty(children), element)
        val linkend: String = linkendAttribute.get(ScalaXml)(element)
        succeed(<a href={s"#$linkend"}>{linkend}</a>)

      // TODO img analogue

      // TODO tgroups etc.; colspan analogue?

      case "informaltable" => succeed(<table>{children}</table>)
      case "row"           => succeed(<tr>{children}</tr>)
      case "entry"         => succeed(<td>{children}</td>)

      case _ => succeed(element)
