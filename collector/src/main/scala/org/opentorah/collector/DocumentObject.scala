package org.opentorah.collector

import org.opentorah.store.WithPath
import org.opentorah.tei.{CalendarDesc, Page, Tei}
import org.opentorah.util.Files
import scala.xml.{Elem, Node}

final class DocumentObject(
  site: Site,
  collection: WithPath[Collection],
  document: Document,
  teiHolder: TeiHolder
) extends SiteObject(site) {

  override protected def htmlUrl: Seq[String] = url(CollectionObject.documentsDirectoryName)

  override def facsUrl: Seq[String] = url(CollectionObject.facsDirectoryName)

  override protected def viewer: Viewer = Viewer.Document

  private def url(directoryName: String): Seq[String] =
    CollectionObject.urlPrefix(collection) ++ Seq(directoryName, teiHolder.name + ".html")

  override protected def tei: Tei = teiHolder.tei

  override protected def headerSummary: Seq[Node] = Seq(
    // TODO here again it seems that the browser ignores the namespace when styling elements
    // that exist in HTML: when I use <p> instead of <ab>, and there is a <p> inside <abstract>,
    // background colour stops before it...
    <div xmlns={Tei.namespace.uri} rendition="document-header">
      <l>{document.description}</l>
      <l>Дата: {document.date}</l>
      <l>Кто: {document.author}</l>
      <l>Кому: {document.addressee}</l>
    </div>)

  override protected def teiTransformer: Tei => Tei =
    Site.addPublicationStatement compose
    Site.addSourceDesc compose
    Tei.addCalendarDesc(new CalendarDesc.Value(<calendar xml:id="julian"><p>Julian calendar</p></calendar>)) compose
    Tei.addLanguage

  override protected def navigationLinks: Seq[NavigationLink] =
    navigation ++
    Seq(NavigationLink(facsUrl, Tei.facsimileSymbol, Some(Viewer.Facsimile))) ++
    (if (teiHolder.language.isDefined || document.languages.isEmpty) Seq.empty
    else document.languages.map(lang => NavigationLink(s"${document.name}-$lang", s"[$lang]", None)))

  // TODO generalize and move into SiteObject
  def facsFile: SiteFile = new SiteFile {
    override def viewer: Viewer = Viewer.Facsimile

    override def url: Seq[String] = facsUrl

    // TODO do pages of the appropriate teiHolder!
    override def contentElement: Elem =
      <div class={Viewer.Facsimile.name}>
        {headerSummary}
        <div class="facsimileScroller">{
          for (page: Page <- document.pages(collection.value.pageType).filterNot(_.pb.isMissing)) yield {
            val n: String = page.pb.n
            val href: Seq[String] = DocumentObject.pageUrl(collection, document.name, page)
            val facs: String = page.pb.facs
              .getOrElse(Site.facsimileBucket + Hierarchy.fileName(collection.value) + "/" + n + ".jpg")
            <a target={Viewer.Document.name} href={Files.mkUrl(href)}>
              <figure>
                <img id={Page.pageId(n)} alt={s"facsimile for page $n"} src={facs}/>
                <figcaption>{n}</figcaption>
              </figure>
            </a>
        }}</div>
      </div>

    override def navigationLinks: Seq[NavigationLink] =
      navigation ++
      Seq(NavigationLink(htmlUrl, "A", Some(Viewer.Document)))
  }

  private def navigation: Seq[NavigationLink] = {
    val (prev: Option[Document], next: Option[Document]) = collection.value.by.get.siblings(document)

    Seq(CollectionObject.navigationLink(collection)) ++
    prev.toSeq.map(prev => NavigationLink(prev.name, "⇦", None)) ++
    Seq(NavigationLink(document.name, document.name, None)) ++
    next.toSeq.map(next => NavigationLink(next.name, "⇨", None))
  }

  override def simpleSubObjects: Seq[SimpleSiteObject] = Seq.empty
}

object DocumentObject {

  // TODO eliminate
  def documentUrl(collection: WithPath[Collection], documentName: String): Seq[String] =
    CollectionObject.urlPrefix(collection) :+ CollectionObject.documentsDirectoryName :+ (documentName + ".html")

  // TODO eliminate
  def pageUrl(collection: WithPath[Collection], documentName: String, page: Page): Seq[String] =
    Files.addPart(documentUrl(collection, documentName), Page.pageId(page.pb.n))

  def resolve(
    site: Site,
    collection: WithPath[Collection],
    parts: Seq[String]
  ): Option[DocumentObject] = if (parts.isEmpty || parts.tail.nonEmpty) None else {
    val (fileName: String, extension: Option[String]) = Files.nameAndExtension(parts.head)
    // Document name can have dots (e.g., 273.2), so if it is referenced without the extension, we end up here -
    // and assume the required extension is implied, and the one found is part of the document name:
    val documentName: String =
      if (extension.isDefined && !extension.contains("html")) parts.head else fileName

    collection.value.findDocumentByName(documentName).map { case (document, teiHolder) =>
      new DocumentObject(site, collection, document, teiHolder)
    }
  }
}
