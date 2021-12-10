package org.opentorah.docbook

import org.opentorah.html.{A, ToHtml}
import org.opentorah.xml.{Attribute, Dialect, Doctype, Namespace, PrettyPrinter, ScalaXml}
import zio.URIO
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

  override protected def elementToHtml(
    element: ScalaXml.Element
  ): URIO[Unit, (ScalaXml.Element, ScalaXml.Nodes)] =
    val name: String = ScalaXml.getName(element)
    val children: ScalaXml.Nodes = ScalaXml.getChildren(element)

    name match
      // TODO link, olink, xref, anchor

      case "ulink" =>
        require(!ScalaXml.isEmpty(children), element)
        succeed(A(URI(urlAttribute.get(ScalaXml)(element)))(children))

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
