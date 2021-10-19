package org.opentorah.collector

import org.opentorah.html
import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Author, Editor, EntityReference, EntityType, Pb, Tei}
import org.opentorah.site.HtmlContent
import org.opentorah.store.{Directory, Path, Terminal}
import org.opentorah.util.Effects
import org.opentorah.xml.{Attribute, Caching, Element, Elements, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO

final class Document(
  override val name: String,
  val isTranslation: Boolean,
  val lang: String,
  val editors: Seq[Editor],
  val description: Option[Abstract.Value],
  val date: Option[String],
  val authors: Seq[Author.Value],
  val addressee: Option[EntityReference],
  val pbs: Seq[Pb]
) extends Directory.Entry(name) derives CanEqual:
  override def equals(other: Any): Boolean =
    val that: Document = other.asInstanceOf[Document]
    this.name == that.name

  def baseName: String = Document.splitLang(name)._1

  def nameWithLang(lang: String): String = s"$baseName-$lang"

  def getDate: ScalaXml.Text = ScalaXml.mkText(date.getOrElse(""))
  def getDescription: ScalaXml.Nodes = description.toSeq.flatMap(_.content.scalaXml)
  def getAuthors: ScalaXml.Nodes = ScalaXml.multi(authors.flatMap(_.content.scalaXml))
  def getAddressee: Seq[ScalaXml.Element] = addressee.toSeq.map(EntityReference.xmlElement)
  def getTranscribers: ScalaXml.Nodes = ScalaXml.multi(editors
    .filter(_.role.contains("transcriber"))
    .flatMap(_.persName)
    .map(EntityReference.xmlElement))

  def pages(pageType: Page.Type): Seq[Page] = pbs.map(pageType(_))

  def textFacetLink(collectionPath: Path, collector: Collector): html.a =
    facetLink(collectionPath, collectionPath.last.asInstanceOf[Collection].textFacet, collector)

  def facetLink(collectionPath: Path, collectionFacet: Collection.CollectionFacet[?], collector: Collector): html.a =
    collector.a(facetPath(collectionPath, collectionFacet))

  def facetPath(
    collectionPath: Path,
    collectionFacet: Collection.CollectionFacet[?],
  ): Path =
    collectionPath ++ Seq(collectionFacet, collectionFacet.of(this))

object Document extends Element[Document]("document"), Directory.EntryMaker[Tei, Document]:

  override def apply(name: String, tei: Tei): Parser[Document] = for
    pbs: Seq[Pb] <- ScalaXml.descendants(tei.text.body.content.scalaXml, Pb.elementName, Pb)
    lang: Option[String] = tei.text.lang
    language: Option[String] = splitLang(name)._2
    _ <- Effects.check(language.isEmpty || language == lang, s"Wrong language in $name: $lang != $language")
    persNames: Seq[EntityReference] <- ScalaXml.descendants(
      nodes = tei.teiHeader.profileDesc.flatMap(_.correspDesc).map(_.content.scalaXml).getOrElse(Seq.empty),
      elementName = EntityType.Person.nameElement,
      elements = EntityReference
    )
  yield new Document(
    name,
    isTranslation = language.isDefined,
    lang = lang.get,
    editors = tei.teiHeader.fileDesc.titleStmt.editors,
    description = tei.teiHeader.profileDesc.flatMap(_.documentAbstract),
    date = tei.teiHeader.profileDesc.flatMap(_.creation.map(_.date)).map(_.when),
    authors = tei.teiHeader.fileDesc.titleStmt.authors,
    addressee = persNames.find(_.role.contains("addressee")),
    pbs = pbs
  )

  private def splitLang(name: String): (String, Option[String]) =
    val dash: Int = name.lastIndexOf('-')
    if (dash == -1) || (dash != name.length-3) then (name, None)
    else (name.substring(0, dash), Some(name.substring(dash+1)))

  private val isTranslationAttribute: Attribute.OrDefault[Boolean] = Attribute.BooleanAttribute("isTranslation").orDefault
  private val langAttribute: Attribute.Required[String] = Attribute("lang").required
  private val dateAttribute: Attribute.Optional[String] = Attribute("date").optional
  private val editorsElement: Elements.Sequence[Editor] = Editor.seq
  private val abstractElement: Elements.Optional[Abstract.Value] = Abstract.element.optional
  private val authorsElement: Elements.Sequence[Author.Value] = Author.element.seq
  private val addresseeElement: Elements.Optional[EntityReference] = EntityReference.optional
  private val pbsElement: Elements.Sequence[Pb] = Pb.seq

  override def contentParsable: Parsable[Document] = new Parsable[Document]:
    override def parser: Parser[Document] = for
      name: String <- Directory.fileNameAttribute()
      isTranslation: Boolean <- isTranslationAttribute()
      lang: String <- langAttribute()
      editors: Seq[Editor] <- editorsElement()
      description: Option[Abstract.Value] <- abstractElement()
      date: Option[String] <- dateAttribute()
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
      dateAttribute(_.date),
      authorsElement(_.authors),
      addresseeElement(_.addressee),
      pbsElement(_.pbs)
    )
