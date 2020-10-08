package org.opentorah.collector

import org.opentorah.store.WithPath
import org.opentorah.tei.{Body, CalendarDesc, Page, Tei}
import org.opentorah.util.Files
import scala.xml.{Elem, Node}

final class DocumentObject(
  site: Site,
  collection: WithPath[Collection],
  document: Document,
  teiHolder: TeiHolder
) extends SiteObject(site) {

  // TODO package extension with the directory name so that it is not repeated in the CollectionObject.resolve...

  override protected def teiUrl: Seq[String] = url(CollectionObject.teiDirectoryName, "xml")

  override protected def teiWrapperUrl: Seq[String] = url(CollectionObject.documentsDirectoryName, "html")

  override protected def htmlUrl: Seq[String] = url(CollectionObject.documentsDirectoryName, "html")

  override protected def facsUrl: Seq[String] = url(CollectionObject.facsDirectoryName, "html")

  override protected def viewer: Viewer = Viewer.Document

  private def url(directoryName: String, extension: String): Seq[String] =
    CollectionObject.urlPrefix(collection) :+ directoryName :+ (teiHolder.name + "." + extension)

  override protected def tei: Tei = {
    val tei = teiHolder.tei
    tei.copy(text = tei.text.copy(body = new Body.Value(headerTei ++ tei.body.xml)))
  }

  private def headerTei: Seq[Node] = {
    // TODO here again it seems that the browser ignores the namespace when styling elements
    // that exist in HTML: when I use <p> instead of <ab>, and there is a <p> inside <abstract>,
    // background colour stops before it...
    <ab xmlns={Tei.namespace.uri} rendition="document-header">
      <l>{document.description}</l>
      <l>Дата: {document.date}</l>
      <l>Кто: {document.author}</l>
      <l>Кому: {document.addressee}</l>
    </ab>
  }

  // TODO links are not live since they are targeting TEI :)
  // Skip the header in facsimile altogether?
  private def headerFacs: Seq[Node] =
    <div class="document-header">
      <span>{document.description}</span><br/>
      <span>Дата: {document.date}</span><br/>
      <span>Кто: {document.author}</span><br/>
      <span>Кому: {document.addressee}</span>
    </div>

  override protected def teiTransformer: Tei => Tei =
    Site.addPublicationStatement compose
    Site.addSourceDesc compose
    Tei.addCalendarDesc(new CalendarDesc.Value(<calendar xml:id="julian"><p>Julian calendar</p></calendar>)) compose
    Tei.addLanguage

  override protected def navigationLinks: Seq[NavigationLink] =
    navigation ++
    Seq(NavigationLink(facsUrl, "⎙", Some(Viewer.Facsimile))) ++
    (if (teiHolder.language.isDefined || document.languages.isEmpty) Seq.empty
    else document.languages.map(lang => NavigationLink(s"${document.name}-$lang", s"[$lang]", None)))

  def facsFile: SiteFile = new HtmlFile {
    override def viewer: Viewer = Viewer.Facsimile

    override protected def siteParameters: SiteParameters = site.siteParameters

    override def url: Seq[String] = facsUrl

    // TODO do pages of the appropriate teiHolder!
    override protected def contentElement: Elem =
      <div class={Viewer.Facsimile.name}>
        {headerFacs}
        <div class="facsimileScroller">
          {for (page: Page <- document.pages(collection.value.pageType).filterNot(_.pb.isMissing)) yield {
          val n: String = page.pb.n
          val href: Seq[String] = DocumentObject.pageUrl(collection, document.name, page)
          val facs: String = page.pb.facs
            .getOrElse(Site.facsimileBucket + Hierarchy.fileName(collection.value) + "/" + n + ".jpg")
          <a target={Viewer.Document.name} href={Files.mkUrl(href)}>
            <figure>
              <img xml:id={Page.pageId(n)} alt={s"facsimile for page $n"} src={facs}/>
              <figcaption>{n}</figcaption>
            </figure>
          </a>
        }}
        </div>
      </div>

    override protected def pageParameters: PageParameters = new PageParameters(
      style = "main",
      navigationLinks =
        navigation ++
        Seq(NavigationLink(teiWrapperUrl, "A", Some(Viewer.Document)))
    )
  }

  private def navigation: Seq[NavigationLink] = {
    val (prev: Option[Document], next: Option[Document]) = collection.value.by.get.siblings(document)

    Seq(CollectionObject.navigationLink(collection)) ++
    prev.toSeq.map(prev => NavigationLink(prev.name, "⇦", None)) ++
    Seq(NavigationLink(document.name, document.name, None)) ++
    next.toSeq.map(next => NavigationLink(next.name, "⇨", None))
  }
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
    parts: Seq[String],
    requiredExtension: String
  ): Option[DocumentObject] = if (parts.isEmpty || parts.tail.nonEmpty) None else {
    val (fileName: String, extension: Option[String]) = Files.nameAndExtension(parts.head)
    // Document name can have dots (e.g., 273.2), so if it is referenced without the extension, we end up here -
    // and assume the required extension is implied, and the one found is part of the document name:
    val documentName: String =
    if (extension.isDefined && !extension.contains(requiredExtension)) parts.head else fileName

    collection.value.findDocumentByName(documentName).map { case (document, teiHolder) =>
      new DocumentObject(site, collection, document, teiHolder)
    }
  }
}
