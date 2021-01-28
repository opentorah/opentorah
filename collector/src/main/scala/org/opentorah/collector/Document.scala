package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Author, Editor, EntityReference, Pb, Tei}
import org.opentorah.xml.{Attribute, Elements, Html, Parsable, Parser, Unparser, Xml}

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

  def getDate: Xml.Text = Xml.mkText(date.getOrElse(""))
  def getDescription: Xml.Nodes = description.toSeq.flatMap(_.xml)
  def getAuthors: Xml.Nodes = Xml.multi(authors.flatMap(_.xml))
  def getAddressee: Seq[Xml.Element] = addressee.toSeq.map(EntityReference.xmlElement)
  def getTranscribers: Xml.Nodes = Xml.multi(editors
    .filter(_.role.contains("transcriber"))
    .flatMap(_.persName)
    .map(EntityReference.xmlElement))

  def pages(pageType: Page.Type): Seq[Page] = for (pb <- pbs) yield pageType(pb)

  def headerSummary: Xml.Element =
    <table xmlns={Html.namespace.uri} class="document-header">
      <tr><td colspan="2">{getDescription}</td></tr>
      <tr><td>Дата</td> <td>{getDate}</td></tr>
      <tr><td>Кто </td> <td>{getAuthors}</td></tr>
      <tr><td>Кому</td> <td>{getAddressee}</td></tr>
    </table>
}

object Document extends Directory.EntryMaker[Tei, Document]("document") {

  sealed abstract class Facet[DF <: Facet[DF, F], F <: Collection.Facet[DF, F]](val document: Document, collectionFacet: F)
    extends Store
  {
    final override def names: Names = Names(withExtension)
    // TODO when dynamized - skip the extension
    final def withExtension: String = document.name + "." + collectionFacet.extension
    final def collection: Collection = collectionFacet.collection
    final def getTei: Tei = collection.getFile(document)
  }

  final class TeiFacet(document: Document, collectionFacet: Collection.TeiFacet)
    extends Facet[TeiFacet, Collection.TeiFacet](document, collectionFacet)

  abstract class HtmlFacet[DF <: HtmlFacet[DF, F], F <: Collection.HtmlFacet[DF, F]](document: Document, collectionFacet: F)
    extends Facet[DF, F](document, collectionFacet) with HtmlContent
  {
    // TODO titles: .orElse(document.tei.titleStmt.titles.headOption.map(_.xml))

    final override def navigationLinks(site: Site): Seq[Xml.Element] = {
      val (prev: Option[Document], next: Option[Document]) = collection.siblings(document)

      collection.navigationLinks(site) ++
      prev.toSeq.map(prev => collectionFacet.of(prev    ).a(site)("⇦"          )) ++
      Seq(                   collectionFacet.of(document).a(site)(document.name)) ++
      next.toSeq.map(next => collectionFacet.of(next    ).a(site)("⇨"          )) ++
      moreNavigationLinks(site)
    }

    protected def moreNavigationLinks(site: Site): Seq[Xml.Element]
  }

  final class TextFacet(document: Document, collectionFacet: Collection.TextFacet)
    extends HtmlFacet[TextFacet, Collection.TextFacet](document, collectionFacet)
  {
    override def viewer: Viewer = Viewer.Document
    override def htmlHeadTitle: Option[String] = None
    override def lang: Option[String] = Some(document.lang)

    override protected def moreNavigationLinks(site: Site): Seq[Xml.Element] =
      Seq(collection.facsimileFacet.of(document).a(site)(text = Tei.facsimileSymbol)) ++ {
        for (translation <- if (document.isTranslation) Seq.empty else collection.translations(document))
        yield collectionFacet.of(translation).a(site)(s"[${translation.lang}]")
      }

    // TODO when dynamized - skip `collection.textFacet`:
    override def path(site: Site): Store.Path =
      collection.path(site) ++ Seq(collection.textFacet, collection.textFacet.of(document))

    override def content(site: Site): Xml.Element =
      <div>
        {document.headerSummary}
        {getTei.body.xml}
      </div>
  }

  final class FacsimileFacet(document: Document, collectionFacet: Collection.FacsimileFacet)
    extends HtmlFacet[FacsimileFacet, Collection.FacsimileFacet](document, collectionFacet)
  {
    override def viewer: Viewer = Viewer.Facsimile
    override def htmlHeadTitle: Option[String] = None

    override protected def moreNavigationLinks(site: Site): Seq[Xml.Element] =
      Seq(collection.textFacet.of(document).a(site)(text = "A"))

    override def path(site: Site): Store.Path =
      collection.path(site) ++ Seq(collection.facsimileFacet, collection.facsimileFacet.of(document))

    override def content(site: Site): Xml.Element =
      <div class={Viewer.Facsimile.name}>
        {document.headerSummary}
        <div class="facsimileScroller">{
          val text: TextFacet = collection.textFacet.of(document)
          val facsimileUrl: String = collection.facsimileUrl(site)

          for (page: Page <- document.pages(collection.pageType).filterNot(_.pb.isMissing)) yield {
            val n: String = page.pb.n
            val pageId: String = Pb.pageId(n)
            text.a(site, part = Some(pageId))(
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
  }

  override def apply(name: String, tei: Tei): Document = {
    val lang: Option[String] = tei.text.lang

    val language: Option[String] = splitLang(name)._2
    if (language.isDefined && lang != language)
      throw new IllegalArgumentException(s"Wrong language in $name: $lang != $language")

    new Document(
      name,
      isTranslation = language.isDefined,
      lang = lang.get,
      editors = tei.titleStmt.editors,
      description = tei.teiHeader.profileDesc.flatMap(_.documentAbstract),
      date = tei.teiHeader.profileDesc.flatMap(_.creation.map(_.date)).map(_.when),
      authors = tei.titleStmt.authors,
      addressee = tei.addressee,
      pbs = tei.pbs
    )
  }

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
