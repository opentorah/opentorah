package org.opentorah.collector

import org.opentorah.store.{Store, WithPath}
import org.opentorah.tei.Tei
import org.opentorah.util.Files
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
    CollectionObject.collectionUrl(collection) :+ directoryName :+ (teiHolder.name + "." + extension)

  override protected def tei: Tei = teiHolder.tei

  override protected def teiTransformer: Tei => Tei = TeiUtil.addCommon

  override protected def yaml: Seq[(String, String)] =
    Seq("facs" -> Site.mkUrl(facsUrl)) ++
    (if (teiHolder.language.isDefined || document.languages.isEmpty) Seq.empty
     else Seq("translations" -> document.languages.mkString("[", ", ", "]"))) ++
    navigation

  private def navigation: Seq[(String, String)] = {
    val (prev: Option[Document], next: Option[Document]) = collection.value.siblings(document)
    Seq("documentCollection" -> Site.collectionReference(collection)) ++
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
            {for (page: Page <- document.pages(collection.value.pageType).filter(_.isPresent); n = page.n) yield {
            <a target={viewer} href={s"../${CollectionObject.documentsDirectoryName}/${document.name}.html#p$n"}>
              <figure>
                <img xml:id={s"p$n"} alt={s"facsimile for page $n"} src={page.facs.orNull}/>
                <figcaption>{n}</figcaption>
              </figure>
            </a>
          }}
          </div>
        </div>

      Site.withYaml(
        yaml = Seq("layout" -> "default", "transcript" -> Site.mkUrl(teiWrapperUrl)) ++ navigation,
        content = Seq(TeiUtil.htmlPrettyPrinter.render(facsimilePages))
      )
    }
  }
}

object DocumentObject {

  val documentViewer: String = "documentViewer"

  def documentUrl(collection: Store, documentName: String): Seq[String] =
    Seq(CollectionObject.collectionsDirectoryName, Site.fileName(collection)) :+ CollectionObject.documentsDirectoryName :+ (documentName + ".html")

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
