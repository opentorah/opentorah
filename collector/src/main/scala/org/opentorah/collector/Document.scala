package org.opentorah.collector

import org.opentorah.html.A
import org.opentorah.tei.{Abstract, Author, Date, Editor, EntityReference, EntityType, Pb, Tei}
import org.opentorah.store.{Context, Directory, Path}
import org.opentorah.util.Effects
import org.opentorah.xml.{Atom, Attribute, ElementTo, Elements, ElementsTo, Nodes, Parsable, Parser, Unparser}

final class Document(
  override val name: String,
  val isTranslation: Boolean,
  val lang: String,
  val editors: Seq[Editor],
  val description: Option[Abstract.Value],
  val date: Option[Date],
  val authors: Seq[Author.Value],
  val addressee: Option[EntityReference],
  val pbs: Seq[Pb]
) extends Directory.Entry(name) derives CanEqual:
  override def equals(other: Any): Boolean =
    val that: Document = other.asInstanceOf[Document]
    this.name == that.name

  def baseName: String = Document.splitLang(name)._1

  def nameWithLang(lang: String): String = s"$baseName-$lang"

  def getDate: Elements = date.toSeq.map(Date.xmlElement)
  def getDescription: Nodes = description.toSeq.flatMap(_.content)
  def getAuthors: Nodes = Nodes.multi(authors.flatMap(_.content))
  def getAddressee: Elements = addressee.toSeq.map(EntityReference.xmlElement)
  def getTranscribers: Nodes = Nodes.multi(editors
    .filter(_.role.contains("transcriber"))
    .flatMap(_.persName)
    .map(EntityReference.xmlElement))

  def pages(pageType: Page.Type): Seq[Page] = pbs.map(pageType(_))

  def textFacetLink(context: Context, collectionPath: Path): Parser[A] =
    facetLink(context, collectionPath, Path.last[Collection](collectionPath).textFacet)

  def facetLink(context: Context, collectionPath: Path, collectionFacet: CollectionFacet): Parser[A] =
    context.a(facetPath(collectionPath, collectionFacet))

  def facetPath(
    collectionPath: Path,
    collectionFacet: CollectionFacet,
  ): Path =
    collectionPath ++ Seq(collectionFacet, collectionFacet.of(this))

object Document extends ElementTo[Document]("document"), Directory.EntryMaker[Tei, Document]:

  override def apply(name: String, tei: Tei): Parser[Document] = for
    pbs: Seq[Pb] <- Pb.descendants(tei.text.body.content, Pb.elementName)
    lang: Option[String] = tei.text.lang
    language: Option[String] = splitLang(name)._2
    _ <- Effects.check(language.isEmpty || language == lang, s"Wrong language in $name: $lang != $language")
    persNames: Seq[EntityReference] <- EntityReference.descendants(
      nodes = tei.teiHeader.profileDesc.flatMap(_.correspDesc).map(_.content).getOrElse(Seq.empty),
      elementName = EntityType.Person.nameElement
    )
  yield new Document(
    name,
    isTranslation = language.isDefined,
    lang = lang.get,
    editors = tei.teiHeader.fileDesc.titleStmt.editors,
    description = tei.teiHeader.profileDesc.flatMap(_.documentAbstract),
    date = tei.teiHeader.profileDesc.flatMap(_.creation).map(_.date).map(forDisplay),
    authors = tei.teiHeader.fileDesc.titleStmt.authors,
    addressee = persNames.find(_.role.contains("addressee")),
    pbs = pbs
  )

  def forDisplay(date: Date): Date = Date(
    when = date.when,
    calendar = date.calendar,
    xml = Seq(Atom(date.when))
  )

  private def splitLang(name: String): (String, Option[String]) =
    val dash: Int = name.lastIndexOf('-')
    if (dash == -1) || (dash != name.length-3) then (name, None)
    else (name.substring(0, dash), Some(name.substring(dash+1)))

  private val isTranslationAttribute: Attribute.OrDefault[Boolean] = Attribute.BooleanAttribute("isTranslation").orDefault
  private val langAttribute: Attribute.Required[String] = Attribute("lang").required
  private val dateElement: ElementsTo.Optional[Date] = Date.optional
  private val editorsElement: ElementsTo.Sequence[Editor] = Editor.seq
  private val abstractElement: ElementsTo.Optional[Abstract.Value] = Abstract.element.optional
  private val authorsElement: ElementsTo.Sequence[Author.Value] = Author.element.seq
  private val addresseeElement: ElementsTo.Optional[EntityReference] = EntityReference.optional
  private val pbsElement: ElementsTo.Sequence[Pb] = Pb.seq

  override def contentParsable: Parsable[Document] = new Parsable[Document]:
    override def parser: Parser[Document] = for
      name: String <- Directory.fileNameAttribute()
      isTranslation: Boolean <- isTranslationAttribute()
      lang: String <- langAttribute()
      editors: Seq[Editor] <- editorsElement()
      description: Option[Abstract.Value] <- abstractElement()
      date: Option[Date] <- dateElement()
      authors: Seq[Author.Value] <- authorsElement()
      addressee: Option[EntityReference] <- addresseeElement()
      pbs: Seq[Pb] <- pbsElement()
    yield new Document(
      name,
      isTranslation,
      lang,
      editors,
      description,
      date,
      authors,
      addressee,
      pbs
    )

    override def unparser: Unparser[Document] = Unparser.concat(
      Directory.fileNameAttribute(_.name),
      isTranslationAttribute(_.isTranslation),
      langAttribute(_.lang),
      editorsElement(_.editors),
      abstractElement(_.description),
      dateElement(_.date),
      authorsElement(_.authors),
      addresseeElement(_.addressee),
      pbsElement(_.pbs)
    )
