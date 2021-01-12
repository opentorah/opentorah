package org.opentorah.tei

import org.opentorah.xml.{Unparser, Attribute, Dialect, Element, From, LinkResolver, Namespace, Parsable, Parser, PrettyPrinter, ToHtml, Xml}
import zio.{URIO, ZIO}

final case class Tei(
  teiHeader: TeiHeader,
  text: Text
) {
  def titleStmt: TitleStmt = teiHeader.fileDesc.titleStmt
  def getAbstract: Option[Abstract.Value] = teiHeader.profileDesc.flatMap(_.documentAbstract)
  def getAbstractXml: Option[Seq[Xml.Node]] = getAbstract.map(_.xml)
  def creationDate: Option[Date] = teiHeader.profileDesc.flatMap(_.creation.map(_.date))
  def languages: Seq[Language] = teiHeader.profileDesc.flatMap(_.langUsage).toSeq.flatMap(_.languages)
  def editors: Seq[Editor] = titleStmt.editors
  def authors: Seq[Author.Value] = titleStmt.authors

  def addressee: Option[EntityReference] =
    EntityReference.from(correspDesc.map(_.xml).getOrElse(Seq.empty))
      .find(name => (name.entityType == EntityType.Person) && name.role.contains("addressee"))

  def pbs: Seq[Pb] = body.xml.flatMap(node =>
    Xml.descendants(node, Pb.elementName)
      .map(descendant => Parser.parseDo(Pb.parse(From.xml("descendants", descendant))))
  )

  // TODO not vals!
  val correspDesc: Option[CorrespDesc.Value] = teiHeader.profileDesc.flatMap(_.correspDesc)
  val body: Body.Value = text.body

  /////  """<?xml-model href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng" schematypens="http://relaxng.org/ns/structure/1.0"?>""" + "\n" +
}

object Tei extends Element[Tei]("TEI") with Dialect with ToHtml {

  override val namespace: Namespace = Namespace(uri = "http://www.tei-c.org/ns/1.0", prefix="tei")

  override val mimeType: String = "application/tei+xml"

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

  def apply(body: Seq[Xml.Node]): Tei = new Tei(
    teiHeader = TeiHeader.empty,
    text = new Text(
      lang = None,
      new Body.Value(body)
    )
  )

  type Transformer = Tei => Tei

  def addPublicationStatement(
    publisher: Publisher.Value,
    status: String,
    licenseName: String,
    licenseUrl: String
  ): Tei.Transformer = tei =>
    tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        publicationStmt = Some(new PublicationStmt(
          publisher = Some(publisher),
          availability = Some(new Availability(
            status = Some(status),
            xml = <licence><ab><ref n="license" target={licenseUrl}>{licenseName}</ref></ab></licence>)))))))

  def addSourceDesc(value: SourceDesc.Value): Tei.Transformer = tei =>
    tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        sourceDesc = Some(value))))

  def addCalendarDesc(value: CalendarDesc.Value): Tei.Transformer = tei =>
    tei.copy(teiHeader = tei.teiHeader.copy(
      profileDesc = Some(tei.teiHeader.profileDesc.getOrElse(ProfileDesc.empty).copy(
        calendarDesc = Some(value)))))

  val addLanguage: Transformer = tei => {
    val textLang: Option[String] = tei.text.lang
    val langUsage: Option[LangUsage] = tei.teiHeader.profileDesc.flatMap(_.langUsage)
    val add: Boolean = langUsage.isEmpty && textLang.isDefined
    if (!add) tei else tei.copy(teiHeader = tei.teiHeader.copy(
      profileDesc = Some(tei.teiHeader.profileDesc.getOrElse(ProfileDesc.empty).copy(langUsage =
        Some(LangUsage(languages = Seq(Language(
          ident = textLang.get,
          usage = None,
          text = None
        ))))))
    ))
  }

  override protected def isEndNote(element: Xml.Element): Boolean =
    (element.label == "note") && placeAttribute.get(element).contains("end")

  private val targetAttribute: Attribute.Required[String] = Attribute("target").required
  private val urlAttribute: Attribute.Required[String] = Attribute("url").required
  private val placeAttribute: Attribute.Optional[String] = Attribute("place").optional
  private val colsAttribute: Attribute.Optional[String] = Attribute("cols").optional

  val facsimileSymbol: String = "⎙"

  def toHtml(resolver: LinkResolver, tei: Tei): Xml.Element = toHtml(resolver, required.xml(tei))

  // - add values of the cssClass attribute to class?
  // - style/rend/rendition?
  // - transform tagsDecl?
  // - transform prefixDef?
  override protected def elementTransform(element: Xml.Element): URIO[State, Xml.Element] = {
    val children: Seq[Xml.Node] = Xml.getChildren(element)

    element.label match {
      case label if EntityType.isName(label) =>
        require(!Xml.isEmpty(children), element)
        link(EntityName.refAttribute.get(element), _.resolver.findByRef(_), children)

      case Ref.elementName =>
        require(!Xml.isEmpty(children))
        link(Some(targetAttribute.get(element)), _.resolver.resolve(_), children)

      case "ptr" =>
        require(Xml.isEmpty(children))
        link(Some(targetAttribute.get(element)), _.resolver.resolve(_), Seq.empty)

      // TODO feed pageId through State to obtain unique id
      // TODO use - do not guess! - ids of the pbs in the facsimile viewer!
      case Pb.elementName =>
        require(Xml.isEmpty(children))
        val pageId: String = Page.pageId(Pb.nAttribute.get(element))
        ZIO.access[State](_.resolver.facs).map(resolved => a(
          id = Some(pageId),
          href = Some(resolved.urlWithPartAsString(pageId)),
          target = resolved.role,
          Seq(Xml.mkText(facsimileSymbol))
        ))

      case "graphic" =>
        // Note: in TEI <graphic> can contain <desc>, but are treating it as empty.
        require(Xml.isEmpty(children))
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
        ZIO.access[State](_.addEndNote(Xml.idAttribute.optional.get(element), children))

      case _ =>
        ZIO.succeed(element)
    }
  }
}
