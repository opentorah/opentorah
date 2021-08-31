package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Author, Editor, EntityReference, EntityType, Pb, Tei}
import org.opentorah.site.HtmlContent
import org.opentorah.store.{Caching, Directory, Store}
import org.opentorah.util.Effects
import org.opentorah.xml.{Attribute, Element, Elements, Parsable, Parser, ScalaXml, Unparser}

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
) extends Directory.Entry(name) {
  def baseName: String = Document.splitLang(name)._1

  def nameWithLang(lang: String): String = s"$baseName-$lang"

  def getDate: ScalaXml.Text = ScalaXml.mkText(date.getOrElse(""))
  def getDescription: ScalaXml.Nodes = description.toSeq.flatMap(_.content)
  def getAuthors: ScalaXml.Nodes = ScalaXml.multi(authors.flatMap(_.content))
  def getAddressee: Seq[ScalaXml.Element] = addressee.toSeq.map(EntityReference.xmlElement)
  def getTranscribers: ScalaXml.Nodes = ScalaXml.multi(editors
    .filter(_.role.contains("transcriber"))
    .flatMap(_.persName)
    .map(EntityReference.xmlElement))

  def pages(pageType: Page.Type): Seq[Page] = pbs.map(pageType(_))
}

object Document extends Element[Document]("document") with Directory.EntryMaker[Tei, Document] {

  sealed abstract class Facet[DF <: Facet[DF, F], F <: Collection.Facet[DF, F]](val document: Document, collectionFacet: F)
    extends Store
  {
    final override def names: Names = Names(document.name)
    final def collection: Collection = collectionFacet.collection
    final def getTei: Caching.Parser[Tei] = collection.getFile(document)
  }

  final class TeiFacet(document: Document, collectionFacet: Collection.TeiFacet)
    extends Facet[TeiFacet, Collection.TeiFacet](document, collectionFacet)

  abstract class HtmlFacet[DF <: HtmlFacet[DF, F], F <: Collection.HtmlFacet[DF, F]](document: Document, val collectionFacet: F)
    extends Facet[DF, F](document, collectionFacet) with HtmlContent[Collector]
  {
    // TODO titles: .orElse(document.tei.titleStmt.titles.headOption.map(_.xml))
  }

  final class TextFacet(document: Document, collectionFacet: Collection.TextFacet)
    extends HtmlFacet[TextFacet, Collection.TextFacet](document, collectionFacet)
  {
    override def htmlHeadTitle: Option[String] = None

    override def content(site: Collector): Caching.Parser[ScalaXml.Element] = for {
      tei <- getTei
      header <- collection.documentHeader(document)
    } yield
      <div>
        {header}
        {tei.text.body.content}
      </div>
  }

  final class FacsimileFacet(document: Document, collectionFacet: Collection.FacsimileFacet)
    extends HtmlFacet[FacsimileFacet, Collection.FacsimileFacet](document, collectionFacet)
  {
    override def htmlHeadTitle: Option[String] = None

    override def content(site: Collector): Caching.Parser[ScalaXml.Element] = collection.documentHeader(document).map(header =>
      <div class="facsimileWrapper">
        {header}
        <div class={Viewer.Facsimile.name}>
          <div class="facsimileScroller">{
            val text: TextFacet = collection.textFacet.of(document)
            val facsimileUrl: String = collection.facsimileUrl(site)
            // TODO generate lists of images and check for missing ones and orphans

            for (page: Page <- document.pages(collection.pageType).filterNot(_.pb.isMissing)) yield {
              val n: String = page.pb.n
              val pageId: String = Pb.pageId(n)
              text.a(site).setFragment(pageId)(
                <figure>
                  <img
                  id={pageId}
                  alt={s"facsimile for page $n"}
                  src={page.pb.facs.getOrElse(s"$facsimileUrl$n.jpg")}
                  />
                  <figcaption>{n}</figcaption>
                </figure>
              )
            }}</div>
        </div>
      </div>
    )
  }

  override def apply(name: String, tei: Tei): Parser[Document] = for {
    pbs <- ScalaXml.descendants(tei.text.body.content, Pb.elementName, Pb)
    lang = tei.text.lang
    language = splitLang(name)._2
    _ <- Effects.check(language.isEmpty || language == lang, s"Wrong language in $name: $lang != $language")
    persNames <- ScalaXml.descendants(
      nodes = tei.teiHeader.profileDesc.flatMap(_.correspDesc).map(_.content).getOrElse(Seq.empty),
      elementName = EntityType.Person.nameElement,
      elements = EntityReference
    )
  } yield new Document(
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

  private def splitLang(name: String): (String, Option[String]) = {
    val dash: Int = name.lastIndexOf('-')
    if ((dash == -1) || (dash != name.length-3)) (name, None)
    else (name.substring(0, dash), Some(name.substring(dash+1)))
  }

  private val isTranslationAttribute: Attribute.OrDefault[Boolean] = new Attribute.BooleanAttribute("isTranslation").orDefault
  private val langAttribute: Attribute.Required[String] = Attribute("lang").required
  private val dateAttribute: Attribute.Optional[String] = Attribute("date").optional
  private val editorsElement: Elements.Sequence[Editor] = Editor.seq
  private val abstractElement: Elements.Optional[Abstract.Value] = Abstract.element.optional
  private val authorsElement: Elements.Sequence[Author.Value] = Author.element.seq
  private val addresseeElement: Elements.Optional[EntityReference] = EntityReference.optional
  private val pbsElement: Elements.Sequence[Pb] = Pb.seq

  override def contentParsable: Parsable[Document] = new Parsable[Document] {
    override def parser: Parser[Document] = for {
      name <- Directory.fileNameAttribute()
      isTranslation <- isTranslationAttribute()
      lang <- langAttribute()
      editors <- editorsElement()
      description <- abstractElement()
      date <- dateAttribute()
      authors <- authorsElement()
      addressee <- addresseeElement()
      pbs <- pbsElement()
    } yield new Document(
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
  }
}
