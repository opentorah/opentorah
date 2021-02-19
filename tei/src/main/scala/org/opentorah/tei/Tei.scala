package org.opentorah.tei

import org.opentorah.util.Files
import org.opentorah.html
import org.opentorah.xml.{Attribute, Dialect, Element, Namespace, Parsable, Parser, PrettyPrinter, Unparser, Xml}
import zio.{Has, URIO, ZLayer}
import java.net.URI

final case class Tei(
  teiHeader: TeiHeader,
  text: Text
)

object Tei extends Element[Tei]("TEI") with Dialect with html.ToHtml[Has[LinksResolver]] {

  override val namespace: Namespace = Namespace(uri = "http://www.tei-c.org/ns/1.0", prefix="tei")

  override val mimeType: String = "application/tei+xml"

  def renderXml(tei: Tei): String = prettyPrinter.renderXml(Tei.xmlElement(tei))

  /////  """<?xml-model href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng" schematypens="http://relaxng.org/ns/structure/1.0"?>""" + "\n" +
  override val prettyPrinter: PrettyPrinter = new PrettyPrinter(
    doNotStackElements = Set("choice"),
    nestElements = Set("p", /*"abstract",*/ "head", "salute", "dateline"),
    clingyElements = Set("note", "lb", "sic", "corr")
  )

  override def contentParsable: Parsable[Tei] = new Parsable[Tei] {
    override lazy val parser: Parser[Tei] = for {
      teiHeader <- TeiHeader.required()
      text <- Text.required()
    } yield new Tei(
      teiHeader,
      text
    )

    override lazy val unparser: Unparser[Tei] = concat(
      TeiHeader.required(_.teiHeader),
      Text.required(_.text)
    )
  }

  def toHtml(linksResolver: LinksResolver, element: Xml.Element): Parser[Xml.Element] =
    toHtml(element).provideLayer(ZLayer.succeed(linksResolver))

  def concat[A](unparsers: Unparser[A]*): Unparser[A] =
    Unparser.concatInNamespace(Tei.namespace, unparsers)

  override protected def isEndNote(element: Xml.Element): Boolean =
    (element.label == "note") && placeAttribute.get(element).contains("end")

  private val targetAttribute: Attribute.Required[String] = Attribute("target").required
  private val urlAttribute: Attribute.Required[String] = Attribute("url").required
  private val placeAttribute: Attribute.Optional[String] = Attribute("place").optional
  private val colsAttribute: Attribute.Optional[String] = Attribute("cols").optional

  val facsimileSymbol: String = "⎙"

  // Note: it is possible to add the tokens from the 'rendition' attribute to the value of the HTML
  // class attribute by augmenting Html.To - but I do not see the need: CSS styling can be applied
  // based on the 'rendition' itself.
  // TEI allows for in-element styling using attribute `style` - and browsers apply CSS from there too!
  override protected def elementTransform(element: Xml.Element): URIO[Has[LinksResolver], Xml.Element] = {
    val children: Xml.Nodes = Xml.getChildren(element)

    element.label match {
      case label if EntityType.isName(label) =>
        require(!Xml.isEmpty(children), element)
        val ref: Option[String] = EntityName.refAttribute.get(element)

        if (ref.isEmpty) URIO.succeed(html.a()(children))
        else URIO.accessM[Has[LinksResolver]](_.get.findByRef(ref.get)).map(_.
          getOrElse(html.a(ref.toSeq))
          (children)
        )

      case "ref" =>
        require(!Xml.isEmpty(children), element)
        reference(element).map(_(children))

      case "ptr" =>
        require(Xml.isEmpty(children), element)
        reference(element).map(_(Seq.empty))

      // TODO feed pageId through State to obtain unique id
      case Pb.elementName =>
        require(Xml.isEmpty(children), element)
        val pageId: String = Pb.pageId(Pb.nAttribute.get(element))
        URIO.accessM[Has[LinksResolver]](_.get.facs(pageId)).map(_
          .getOrElse(html.a(Seq(pageId)))
          .copy(id = Some(pageId))
          (text = facsimileSymbol)
        )

      case "graphic" =>
        // Note: in TEI <graphic> can contain <desc>, but we are treating it as empty.
        require(Xml.isEmpty(children), element)
        URIO.succeed(<img src={urlAttribute.get(element)}/>)

      case "table" =>
        URIO.succeed(<table>{children}</table>)

      // Note: before the first row there can be <head>HEAD</head>;
      // it should become <caption>transform(HEAD)</caption>.
      case "row" =>
        URIO.succeed(<tr>{children}</tr>)

      case "cell" =>
        URIO.succeed(<td colspan={colsAttribute.get(element).orNull}>{children}</td>)

      case _ =>
        URIO.succeed(element)
    }
  }

  private def reference(element: Xml.Element): URIO[Has[LinksResolver], html.a] = {
    val uri: URI = new URI(targetAttribute.get(element))

    // TODO maybe just call up regardless?
    if (uri.isAbsolute) URIO.succeed(html.a(uri))
    else URIO.accessM[Has[LinksResolver]](_.get.resolve(Files.splitUrl(uri.getPath))).map(_
        .map(a => Option(uri.getFragment).fold(a)(a.setFragment))
        .getOrElse(html.a(uri))
      )
  }
}
