package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Author, Editor, EntityReference, Page, Pb, Tei}
import org.opentorah.util.Files
import org.opentorah.xml.{Unparser, Attribute, Elements, Parsable, Parser, Xml}

final class Document(
  override val name: String,
  val lang: String,
  val editors: Seq[Editor],
  val description: Option[Abstract.Value],
  val date: Option[String],
  val authors: Seq[Author.Value],
  val addressee: Option[EntityReference],
  val pbs: Seq[Pb]
) extends Directory.Entry(name) {

  def pages(pageType: Page.Type): Seq[Page] =
    for (pb <- pbs) yield pageType(pb)
}

object Document extends Directory.EntryMaker[Tei, Document]("document") {

  sealed abstract class Facet[F <: Collection.Facet](document: Document, collectionFacet: F) extends Store {
    final override def findByName(name: String): Option[Store] = None
    final override def names: Names = Names(withExtension)
    final def withExtension: String = document.name + "." + collectionFacet.extension
    final def collection: Collection = collectionFacet.collection
  }

  final class TeiFacet(document: Document, collectionFacet: Collection.TeiFacet)
    extends Facet[Collection.TeiFacet](document, collectionFacet)
  {
    // TODO add owner etc.
    def content(site: Site): Xml.Element =
      Tei.required.xml(collection.getFile(document))
  }

  final class HtmlFacet(document: Document, collectionFacet: Collection.HtmlFacet)
    extends Facet[Collection.HtmlFacet](document, collectionFacet) with HtmlContent
  {
    override def viewer: Html.Viewer = Html.Viewer.Document
    override def isWide: Boolean = false
    override def htmlTitle: Option[String] = None // TODO
    override def navigationLinks: Seq[Html.NavigationLink] = Seq.empty // TODO
    override def lang: Option[String] = Some(document.lang)

    // TODO add owner/calendar etc.
    // TODO add header summary and title to the text
    override def content(site: Site): Xml.Element =
      Tei.required.xml(collection.getFile(document))
  }

  final class FacsFacet(document: Document, collectionFacet: Collection.FacsFacet)
    extends Facet[Collection.FacsFacet](document, collectionFacet) with HtmlContent
  {
    override def viewer: Html.Viewer = Html.Viewer.Facsimile
    override def isWide: Boolean = false
    override def htmlTitle: Option[String] = None // TODO
    override def navigationLinks: Seq[Html.NavigationLink] = Seq.empty // TODO
    override def lang: Option[String] = None

    override def content(site: Site): Xml.Element =
      <div class={Html.Viewer.Facsimile.name}>
        {site.headerSummary(collection)}
        <div class="facsimileScroller">{
          for (page: Page <- document.pages(collection.pageType).filterNot(_.pb.isMissing)) yield {
            val n: String = page.pb.n
            <a target={Html.Viewer.Document.name} href={Files.mkUrl(site.pageUrl(collection, document, page))}>
              <figure>
                <img
                id={Page.pageId(n)}
                alt={s"facsimile for page $n"}
                src={page.pb.facs.getOrElse(site.facsimilesUrl + site.collectionUrl(collection) + "/" + n + ".jpg")}
                />
                <figcaption>{n}</figcaption>
              </figure>
            </a>
          }}</div>
      </div>
  }

  override def apply(name: String, tei: Tei): Document = {
    val lang: Option[String] = tei.text.lang

    val (_: String, language: Option[String]) = splitLang(name)
    if (language.isDefined && lang != language)
      throw new IllegalArgumentException(s"Wrong language in $name: $lang != $language")

    new Document(
      name,
      lang.get,
      tei.editors,
      description = tei.getAbstract,
      date = tei.creationDate.map(_.when),
      tei.authors,
      tei.addressee,
      tei.pbs
    )
  }

  private def splitLang(name: String): (String, Option[String]) = {
    val dash: Int = name.lastIndexOf('-')
    if ((dash == -1) || (dash != name.length-3)) (name, None)
    else (name.substring(0, dash), Some(name.substring(dash+1)))
  }

  private val langAttribute: Attribute.Required[String] = Attribute("lang").required
  private val dateAttribute: Attribute.Optional[String] = Attribute("date").optional
  private val editorsElement: Elements.Sequence[Editor] = Editor.seq
  private val abstractElement: Elements.Optional[Abstract.Value] = Abstract.element.optional
  private val authorsElement: Elements.Sequence[Author.Value] = Author.element.seq
  private val entityReferenceElement: Elements.Optional[EntityReference] = EntityReference.optional
  private val pbsElement: Elements.Sequence[Pb] = Pb.seq

  override def contentParsable: Parsable[Document] = new Parsable[Document] {
    override def parser: Parser[Document] = for {
      name <- Directory.fileNameAttribute()
      lang <- langAttribute()
      editors <- editorsElement()
      description <- abstractElement()
      date <- dateAttribute()
      authors <- authorsElement()
      addressee <- entityReferenceElement()
      pbs <- pbsElement()
    } yield new Document(
      name,
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
      langAttribute(_.lang),
      editorsElement(_.editors),
      abstractElement(_.description),
      dateAttribute(_.date),
      authorsElement(_.authors),
      entityReferenceElement(_.addressee),
      pbsElement(_.pbs)
    )
  }
}
