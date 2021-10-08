package org.opentorah.tei

import org.opentorah.util.Files
import org.opentorah.html
import org.opentorah.xml.{Attribute, Dialect, Element, Namespace, Parsable, Parser, PrettyPrinter, ScalaXml, Unparser}
import zio.{Has, URIO}
import java.net.URI

final class Tei(
  val teiHeader: TeiHeader,
  val text: Text
):
  def copy(
    teiHeader: TeiHeader = teiHeader,
    text: Text = text
  ): Tei = Tei(
    teiHeader,
    text
  )

object Tei extends Element[Tei]("TEI"), Dialect, html.ToHtml[Has[LinksResolver]]:

  override val namespace: Namespace = Namespace(uri = "http://www.tei-c.org/ns/1.0", prefix="tei")

  override val mimeType: String = "application/tei+xml"

  /////  """<?xml-model href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng" schematypens="http://relaxng.org/ns/structure/1.0"?>""" + "\n" +
  override val prettyPrinter: PrettyPrinter = PrettyPrinter(
    doNotStackElements = Set("choice"),
    nestElements = Set("p", /*"abstract",*/ "head", "salute", "dateline"),
    clingyElements = Set("note", "lb", "sic", "corr")
  )

  override def contentParsable: Parsable[Tei] = new Parsable[Tei]:
    override lazy val parser: Parser[Tei] = for
      teiHeader: TeiHeader <- TeiHeader.required()
      text: Text <- Text.required()
    yield Tei(
      teiHeader,
      text
    )

    override lazy val unparser: Unparser[Tei] = concat(
      TeiHeader.required(_.teiHeader),
      Text.required(_.text)
    )

  def concat[A](unparsers: Unparser[A]*): Unparser[A] =
    Unparser.concatInNamespace(Tei.namespace, unparsers)

  override protected def isEndNote(element: ScalaXml.Element): Boolean =
    (ScalaXml.getName(element) == "note") && placeAttribute.get(ScalaXml)(element).contains("end")

  private val targetAttribute: Attribute.Required[String] = Attribute("target").required
  private val urlAttribute: Attribute.Required[String] = Attribute("url").required
  private val placeAttribute: Attribute.Optional[String] = Attribute("place").optional
  private val colsAttribute: Attribute.Optional[String] = Attribute("cols").optional

  val facsimileSymbol: String = "⎙"

  // Note: it is possible to add the tokens from the 'rendition' attribute to the value of the HTML
  // class attribute by augmenting Html.To - but I do not see the need: CSS styling can be applied
  // based on the 'rendition' itself.
  // TEI allows for in-element styling using attribute `style` - and browsers apply CSS from there too!
  override protected def elementTransform(element: ScalaXml.Element): URIO[Has[LinksResolver], ScalaXml.Element] =
    val children: ScalaXml.Nodes = ScalaXml.getChildren(element)

    ScalaXml.getName(element) match
      case label if EntityType.isName(label) =>
        require(!ScalaXml.isEmpty(children), element)
        val ref: Option[String] = EntityName.refAttribute.get(ScalaXml)(element)

        if ref.isEmpty then URIO.succeed(html.a.empty(children))
        else URIO.accessM[Has[LinksResolver]](_.get.findByRef(ref.get)).map(_.
          getOrElse(html.a(ref.toSeq))
          (children)
        )

      case "ref" =>
        require(!ScalaXml.isEmpty(children), element)
        reference(element).map(_(children))

      case "ptr" =>
        require(ScalaXml.isEmpty(children), element)
        reference(element).map(_(Seq.empty))

      // TODO feed pageId through State to obtain unique id
      case Pb.elementName =>
        require(ScalaXml.isEmpty(children), element)
        val pageId: String = Pb.pageId(Pb.nAttribute.get(ScalaXml)(element))
        URIO.accessM[Has[LinksResolver]](_.get.facs(pageId)).map(_
          .getOrElse(html.a(Seq(pageId)))
          .setId(pageId)
          (text = facsimileSymbol)
        )

      case "graphic" =>
        // Note: in TEI <graphic> can contain <desc>, but we are treating it as empty.
        require(ScalaXml.isEmpty(children), element)
        URIO.succeed(<img src={urlAttribute.get(ScalaXml)(element)}/>)

      case "table" =>
        URIO.succeed(<table>{children}</table>)

      // Note: before the first row there can be <head>HEAD</head>;
      // it should become <caption>transform(HEAD)</caption>.
      case "row" =>
        URIO.succeed(<tr>{children}</tr>)

      case "cell" =>
        URIO.succeed(<td colspan={colsAttribute.get(ScalaXml)(element).orNull}>{children}</td>)

      case _ =>
        URIO.succeed(element)

  private def reference(element: ScalaXml.Element): URIO[Has[LinksResolver], html.a] =
    val uri: URI = URI(targetAttribute.get(ScalaXml)(element))

    // TODO maybe just call up regardless?
    if uri.isAbsolute then URIO.succeed(html.a(uri))
    else URIO.accessM[Has[LinksResolver]](_.get.resolve(Files.splitUrl(uri.getPath))).map(_
        .map(a => Option(uri.getFragment).fold(a)(a.setFragment))
        .getOrElse(html.a(uri))
      )
