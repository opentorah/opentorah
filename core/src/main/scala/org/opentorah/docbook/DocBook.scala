package org.opentorah.docbook

import org.opentorah.html.{A, ToHtml}
import org.opentorah.xml.{Attribute, Dialect, Doctype, Namespace, PrettyPrinter, ScalaXml}
import zio.ZIO
import java.net.URI

object DocBook extends Dialect, Doctype, ToHtml[Unit]:

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
  )

  val version: String = "5.0"

  val equationElements: Set[String] = Set("equation", "informalequation", "inlineequation")

  val inlineEquationElements: Set[String] = Set("inlineequation")

  // Elements that contain code
  val codeElements: Set[String] = Set()

  override val prettyPrinter: PrettyPrinter = PrettyPrinter(
    alwaysStackElements =
      Set("book", "part", "article", "para") ++ //"itemizedlist",
      equationElements ++
      Seq("math", "mrow", "mi"), // TODO from MathML
    clingyElements = Set("footnote")
  )

  override protected def isFootnote(element: ScalaXml.Element): Boolean = ScalaXml.getName(element) == "footnote"

  private val footnotesContainers: Set[String] = Set("chapter")

  override protected def isFootnotesContainer(element: ScalaXml.Element): Boolean =
    footnotesContainers.contains(ScalaXml.getName(element))

  private val urlAttribute: Attribute.Required[String] = Attribute("url").required
  private val linkendAttribute: Attribute.Required[String] = Attribute("linkend").required

  override protected def elementToHtml(element: ScalaXml.Element): zio.URIO[Unit, ScalaXml.Element] =
    val children: ScalaXml.Nodes = ScalaXml.getChildren(element)

    ZIO.succeed(ScalaXml.getName(element) match
      // anchor: empty anchor
      // xref: empty; text to show:
      //   content of the element with the id 'endterm' or
      //   'xreflabel' of the element with id 'linkend' or
      //   (something based on) 'xrefstyle'
      // link; text to show:
      //   if non-empty - content; else - content of the element pointed at by 'endterm'
      //   link to:
      //   if 'linkend' is present - a link to the id
      //   if 'xlink:href' is present - a link to the URI (ulink replacement)
      // biblioref: linkend...
      // olink: TLDR

      // TODO footnote/footnoteref: assign labels to footnotes first or retrieve them from the resulting anchors?
      // TODO is there footnoteref analogue in TEI?

      // TODO remove
      case "ulink" =>
        require(!ScalaXml.isEmpty(children), element)
        ToHtml.namespace(A(URI(urlAttribute.get(ScalaXml)(element)))(children))

      // TODO look up the reference in the bibliography entry itself
      // TODO add chunk name for chunked mode
      case "biblioref" =>
        require(ScalaXml.isEmpty(children), element)
        val linkend: String = linkendAttribute.get(ScalaXml)(element)
        ToHtml.namespace(<a href={s"#$linkend"}>{linkend}</a>)

      // TODO img analogue

      // TODO tgroups etc.; colspan analogue?

      case "informaltable" => org.opentorah.html.Html.table(children)
      case "row"           => org.opentorah.html.Html.tr(children)
      case "entry"         => org.opentorah.html.Html.td(None, children)

      case _ => element
    )
