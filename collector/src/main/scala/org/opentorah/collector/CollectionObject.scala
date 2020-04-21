package org.opentorah.collector

import org.opentorah.entity.EntityReference
import org.opentorah.store.WithPath
import org.opentorah.tei.Ref
import org.opentorah.util.{Files, Xml}
import scala.xml.{Elem, Node}

final class CollectionObject(site: Site, collection: WithPath[Collection]) extends SimpleSiteObject(site) {

  override protected def urlPrefix: Seq[String] = CollectionObject.urlPrefix(collection)

  override protected def fileName: String = CollectionObject.fileName

  override protected def teiWrapperViewer: Viewer = Viewer.Collection

  override protected def yaml: Seq[(String, String)] = Seq(
    "style" -> "wide",
    "documentCollection" -> Hierarchy.storeName(collection.value)
  )

  override protected def teiBody: Seq[Node] = {
    val missingPages: Seq[String] = collection.value.documents
      .flatMap(document => document.pages(collection.value.pageType))
      .filter(_.pb.isMissing)
      .map(_.displayName)

    Hierarchy.storeHeader(collection.path, collection.value) ++
    Seq[Elem](CollectionObject.table(collection).toTei(
      collection.value.parts.flatMap { part =>
          part.title.fold[Seq[Node]](Seq.empty)(_.xml).map(Table.Xml) ++
          part.documents.map(Table.Data[Document]) }
    )) ++
    (if (missingPages.isEmpty) Seq.empty
     else Seq(<p>Отсутствуют фотографии {missingPages.length} страниц: {missingPages.mkString(" ")}</p>))
  }
}

object CollectionObject {

  val fileName: String = "index"

  def urlPrefix(collection: WithPath[Collection]): Seq[String] =
    Seq(CollectionObject.directoryName, Hierarchy.fileName(collection.value))

  def teiWrapperUrl(collection: WithPath[Collection]): Seq[String] =
    urlPrefix(collection) :+ (fileName + ".html")

  // Note: also hard-coded in 'index.xml'!
  val directoryName: String = "collections"

  val facsDirectoryName: String = "facs" // facsimile viewers

  val documentsDirectoryName: String = "documents" // wrappers for TEI XML

  val teiDirectoryName: String = "tei"

  def resolve(site: Site, parts: Seq[String]): Option[SiteFile] = if (parts.isEmpty) None else
    site.findCollectionByName(parts.head).flatMap { collection =>
      if (parts.tail.isEmpty) Some(new CollectionObject(site, collection).teiWrapperFile)
      else parts.tail.head match {
        case CollectionObject.documentsDirectoryName =>
          DocumentObject.resolve(site, collection, parts.tail.tail, "html").map(_.teiWrapperFile)
        case CollectionObject.teiDirectoryName =>
          DocumentObject.resolve(site, collection, parts.tail.tail, "xml").map(_.teiFile)
        case CollectionObject.facsDirectoryName =>
          DocumentObject.resolve(site, collection, parts.tail.tail, "html").map(_.facsFile)

        case file => if (parts.tail.tail.nonEmpty) None else {
          val (fileName: String, extension: Option[String]) = Files.nameAndExtension(file)
          if (fileName == CollectionObject.fileName)
            SimpleSiteObject.resolve(extension, new CollectionObject(site, collection))
          else
            DocumentObject.resolve(site, collection, parts.tail, "html").map(_.teiWrapperFile)
        }
      }
    }

  def table(collection: WithPath[Collection]): Table[Document] = new Table[Document](
    Table.Column("Описание", "description", _.description),
    Table.Column("Дата", "date", _.date),
    Table.Column("Кто", "author", _.author),
    Table.Column("Кому", "addressee", _.addressee),

    Table.Column("Язык", "language", { document: Document =>
      val translations: Seq[Elem] =
        for (teiHolder <- document.by.get.stores.filter(_.language.isDefined))
        yield Ref.toXml(DocumentObject.documentUrl(collection, teiHolder.name), teiHolder.language.get)
      val language: Option[String] = document.tei.languages.map(_.ident).headOption
        .orElse(document.tei.text.lang)
      Seq(Xml.textNode(language.getOrElse("?"))) ++ translations.flatMap(r => Seq(Xml.textNode(" "), r))
    }),

    Table.Column("Документ", "document", { document: Document =>
      Ref.toXml(DocumentObject.documentUrl(collection, document.name), document.name)
    }),

    Table.Column("Страницы", "pages", { document: Document =>
      for (page <- document.pages(collection.value.pageType)) yield Ref.toXml(
        target = DocumentObject.pageUrl(collection, document.name, page),
        text = page.displayName,
        rendition = Some(Page.pageRendition(page.pb.isMissing))
      )
    }),

    Table.Column("Расшифровка", "transcriber", { document: Document =>
      val transcribers = document.tei.titleStmt.editors
        .filter(_.role.contains("transcriber")).flatMap(_.persName)
        .map(transcriber => Xml.removeNamespace(EntityReference.toXml(transcriber)))
      Transformers.multi(transcribers)
    })
  )
}
