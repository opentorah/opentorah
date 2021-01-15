package org.opentorah.tei

import org.opentorah.xml.{Attribute, Dialect, Element, From, Html, Namespace, Parsable, Parser, PrettyPrinter, Unparser, Xml}
import zio.{URIO, ZIO}

final case class Tei(
  teiHeader: TeiHeader,
  text: Text
) {
  def titleStmt: TitleStmt = teiHeader.fileDesc.titleStmt

  def addressee: Option[EntityReference] =
    EntityReference.from(correspDesc.map(_.xml).getOrElse(Seq.empty))
      .find(name => (name.entityType == EntityType.Person) && name.role.contains("addressee"))

  def pbs: Seq[Pb] = body.xml.flatMap(node =>
    Xml.descendants(node, Pb.elementName)
      .map(descendant => Parser.parseDo(Pb.parse(From.xml("descendants", descendant))))
  )

  def correspDesc: Option[CorrespDesc.Value] = teiHeader.profileDesc.flatMap(_.correspDesc)
  def body: Body.Value = text.body

  /////  """<?xml-model href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng" schematypens="http://relaxng.org/ns/structure/1.0"?>""" + "\n" +
}

object Tei extends Element[Tei]("TEI") with Dialect with Html.To {

  override val namespace: Namespace = Namespace(uri = "http://www.tei-c.org/ns/1.0", prefix="tei")

  override val mimeType: String = "application/tei+xml"

  def renderXml(tei: Tei): String = prettyPrinter.renderXml(Tei.xmlElement(tei))

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

  def concat[A](unparsers: Unparser[A]*): Unparser[A] =
    Unparser.concatInNamespace(Tei.namespace, unparsers)

  override protected def isEndNote(element: Xml.Element): Boolean =
    (element.label == "note") && placeAttribute.get(element).contains("end")

  private val targetAttribute: Attribute.Required[String] = Attribute("target").required
  private val urlAttribute: Attribute.Required[String] = Attribute("url").required
  private val placeAttribute: Attribute.Optional[String] = Attribute("place").optional
  private val colsAttribute: Attribute.Optional[String] = Attribute("cols").optional

  val facsimileSymbol: String = "⎙"

  // Note: it is possible to add the tokens from the 'rendition' attribute to the value of the HTML class attribute
  // by augmenting Html.To - but I do not see the need: CSS styling can be applied based on the 'rendition' itself.
  // but I do not see the need
  override protected def elementTransform(element: Xml.Element): URIO[Html.State, Xml.Element] = {
    val children: Seq[Xml.Node] = Xml.getChildren(element)

    element.label match {
      case label if EntityType.isName(label) =>
        require(!Xml.isEmpty(children), element)
        link(EntityName.refAttribute.get(element), _.findByRef(_)).map(_(children))

      case Ref.elementName =>
        require(!Xml.isEmpty(children), element)
        link(Some(targetAttribute.get(element)), _.resolve(_)).map(_(children))

      case "ptr" =>
        require(Xml.isEmpty(children), element)
        link(Some(targetAttribute.get(element)), _.resolve(_)).map(_(Seq.empty))

      // TODO feed pageId through State to obtain unique id
      case Pb.elementName =>
        require(Xml.isEmpty(children), element)
        val pageId: String = Pb.pageId(Pb.nAttribute.get(element))
        link(Some(pageId), _.facs(_), id = Some(pageId)).map(_(facsimileSymbol))

      case "graphic" =>
        // Note: in TEI <graphic> can contain <desc>, but we are treating it as empty.
        require(Xml.isEmpty(children), element)
        ZIO.succeed(<img src={urlAttribute.get(element)}/>)

      case "table" =>
        ZIO.succeed(<table>{children}</table>)

      // Note: before the first row there can be <head>HEAD</head>;
      // it should become <caption>transform(HEAD)</caption>.
      case "row" =>
        ZIO.succeed(<tr>{children}</tr>)

      case "cell" =>
        ZIO.succeed(<td colspan={colsAttribute.get(element).orNull}>{children}</td>)

      case _ if isEndNote(element) =>
        ZIO.access[Html.State](_.addEndNote(Xml.idAttribute.optional.get(element), children))

      case _ =>
        ZIO.succeed(element)
    }
  }
}
