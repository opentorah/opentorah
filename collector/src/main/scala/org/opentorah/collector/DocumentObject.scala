package org.opentorah.collector

import org.opentorah.store.WithPath
import org.opentorah.tei.Tei
import org.opentorah.util.{Files, Xml}
import scala.xml.Elem

final class DocumentObject(
  site: Site,
  collection: WithPath[Collection],
  document: Document,
  teiHolder: TeiHolder
) extends SiteObject(site) {
  override def viewer: String = DocumentObject.documentViewer

  override protected def teiUrl: Seq[String] = url(CollectionObject.teiDirectoryName, "xml")

  override protected def teiWrapperUrl: Seq[String] = url(CollectionObject.documentsDirectoryName, "html")

  private def facsUrl: Seq[String] = url(CollectionObject.facsDirectoryName, "html")

  private def url(directoryName: String, extension: String): Seq[String] =
    CollectionObject.urlPrefix(collection) :+ directoryName :+ (teiHolder.name + "." + extension)

  override protected def tei: Tei = teiHolder.tei

  override protected def teiTransformer: Tei => Tei = Transformations.addCommon

  override protected def xmlTransformer: Xml.Transformer =
    Transformations.refRoleRewriter(site) compose Transformations.pbTransformer(facsUrl)

  override protected def yaml: Seq[(String, String)] =
    Seq("facs" -> Files.mkUrl(facsUrl)) ++
    (if (teiHolder.language.isDefined || document.languages.isEmpty) Seq.empty
     else Seq("translations" -> document.languages.mkString("[", ", ", "]"))) ++
    navigation

  private def navigation: Seq[(String, String)] = {
    val (prev: Option[Document], next: Option[Document]) = collection.value.siblings(document)
    Seq("documentCollection" -> CollectionObject.collectionReference(collection)) ++
    prev.map(prev => Seq("prevDocument" -> prev.name)).getOrElse(Seq.empty) ++
    Seq("thisDocument" -> document.name) ++
    next.map(next => Seq("nextDocument" -> next.name)).getOrElse(Seq.empty)
  }

  def facsFile: SiteFile = new SiteFile {
    override def siteObject: SiteObject = DocumentObject.this

    override def url: Seq[String] = facsUrl

    override def content: String = {
      // TODO do pages of the appropriate teiHolder!
      val facsimilePages: Elem =
        <div class="facsimileViewer">
          <div class="facsimileScroller">
            {for (page: Page <- document.pages(collection.value.pageType).filterNot(_.pb.isMissing)) yield {
            val n: String = page.pb.n
            val href: Seq[String] = DocumentObject.pageUrl(collection, document.name, page)
            val facs: String = page.pb.facs
              .getOrElse(Site.facsimileBucket + Site.fileName(collection.value) + "/" + n + ".jpg")
            <a target={viewer} href={Files.mkUrl(href)}>
              <figure>
                <img xml:id={Page.pageId(n)} alt={s"facsimile for page $n"} src={facs}/>
                <figcaption>{n}</figcaption>
              </figure>
            </a>
          }}
          </div>
        </div>

      SiteObject.withYaml(
        yaml = Seq("layout" -> "default", "transcript" -> Files.mkUrl(teiWrapperUrl)) ++ navigation,
        content = Seq(Transformations.htmlPrettyPrinter.render(facsimilePages))
      )
    }
  }
}

object DocumentObject {

  val documentViewer: String = "documentViewer"

  val facsimileViewer: String = "facsimileViewer"

  def documentUrl(collection: WithPath[Collection], documentName: String): Seq[String] =
    CollectionObject.urlPrefix(collection) :+ CollectionObject.documentsDirectoryName :+ (documentName + ".html")

  def pageUrl(collection: WithPath[Collection], documentName: String, page: Page): Seq[String] =
    Files.addPart(documentUrl(collection, documentName), Page.pageId(page.pb.n))

  def resolve(
    site: Site,
    collection: WithPath[Collection],
    parts: Seq[String],
    requiredExtension: String
  ): Option[DocumentObject] = if (parts.isEmpty || parts.tail.nonEmpty) None else {
    val (fileName: String, extension: Option[String]) = Files.nameAndExtension(parts.head)
    if (!extension.contains(requiredExtension)) None else {
      collection.value.findDocumentByName(fileName).map { case (document, teiHolder) =>
        new DocumentObject(site, collection, document, teiHolder)
      }
    }
  }
}
