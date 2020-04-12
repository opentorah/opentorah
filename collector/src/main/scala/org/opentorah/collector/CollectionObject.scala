package org.opentorah.collector

import org.opentorah.entity.EntityReference
import org.opentorah.store.WithPath
import org.opentorah.tei.Tei
import org.opentorah.util.Files
import org.opentorah.xml.XmlUtil
import scala.xml.{Elem, Node}

final class CollectionObject(site: Site, collection: WithPath[Collection]) extends SimpleSiteObject(site) {
  override def viewer: String = CollectionObject.collectionViewer

  override protected def urlPrefix: Seq[String] = CollectionObject.urlPrefix(collection)

  override protected def fileName: String = CollectionObject.fileName

  override protected def style: Option[String] = Some("wide")

  override protected def yaml: Seq[(String, String)] =
    Seq("documentCollection" -> Site.collectionReference(collection))

  override protected def tei: Tei = {
    val missingPages: Seq[String] = collection.value.documents
      .flatMap(document => document.pages(collection.value.pageType))
      .filterNot(_.isPresent)
      .map(_.displayName)

    val result =
      <head>{Site.collectionTitle(collection)}</head> ++
      Site.collectionDescription(collection) ++
      Seq[Elem](CollectionObject.table(collection).toTei(
        collection.value.parts.flatMap { part =>
          part.title.fold[Seq[Node]](Seq.empty)(_.xml).map(Table.Xml) ++
          part.documents.map(Table.Data[Document]) }
      )) ++
      (if (missingPages.isEmpty) Seq.empty
      else Seq(<p>Отсутствуют фотографии {missingPages.length} страниц: {missingPages.mkString(" ")}</p>))

    Tei(result)
  }
}

object CollectionObject {

  val fileName: String = "index"

  def urlPrefix(collection: WithPath[Collection]): Seq[String] =
    Seq(CollectionObject.collectionsDirectoryName, Site.fileName(collection.value))

  def teiWrapperUrl(collection: WithPath[Collection]): Seq[String] =
    urlPrefix(collection) :+ (fileName + ".html")

  // Note: also hard-coded in 'index.xml'!
  val collectionsDirectoryName: String = "collections"

  val collectionViewer: String = "collectionViewer"

  // Note: also hard-coded in _layouts/tei.html!
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
          if (fileName != fileName) None
          else SimpleSiteObject.resolve(extension, new CollectionObject(site, collection))
        }
      }
    }

  def table(collection: WithPath[Collection]): Table[Document] = new Table[Document](
    Table.Column("Описание", "description", { document: Document =>
      document.tei.getAbstract
        // Ignoring the titles:          .orElse(document.tei.titleStmt.titles.headOption.map(_.xml))
        .getOrElse(Seq.empty)
        .map(XmlUtil.removeNamespace)
    }),

    Table.Column("Дата", "date", { document: Document =>
      Site.textNode(document.tei.creationDate.map(_.when).getOrElse(""))
    }),

    Table.Column("Кто", "author", { document: Document =>
      val authors = document.tei.titleStmt.authors.map(_.xml).flatMap(_.map(XmlUtil.removeNamespace))
      Site.multi(authors)
    }),

    Table.Column("Кому", "addressee",  { document =>
      document.tei.addressee.fold[Seq[Node]](Site.textNode(""))(addressee =>
        <persName ref={addressee.ref.orNull}>{addressee.name}</persName>)
    }),

    Table.Column("Язык", "language", { document: Document =>
      val translations: Seq[Elem] =
        for (teiHolder <- document.by.get.stores.filter(_.language.isDefined))
        yield Site.ref(DocumentObject.documentUrl(collection, teiHolder.name), teiHolder.language.get)
      val language: Option[String] = document.tei.languages.map(_.ident).headOption
        .orElse(document.tei.text.lang)
      Seq(Site.textNode(language.getOrElse("?"))) ++ translations
    }),

    Table.Column("Документ", "document", { document: Document =>
      Site.ref(DocumentObject.documentUrl(collection, document.name), document.name)
    }),

    Table.Column("Страницы", "pages", { document: Document =>
      for (page <- document.pages(collection.value.pageType))
      yield Site.ref(Site.addPart(DocumentObject.documentUrl(collection, document.name), s"p${page.n}"), page.displayName,
        Some(if (page.isPresent) "page" else "missing-page"))
    }),

    Table.Column("Расшифровка", "transcriber", { document: Document =>
      val transcribers = document.tei.titleStmt.editors
        .filter(_.role.contains("transcriber")).flatMap(_.persName)
        .map(transcriber => XmlUtil.removeNamespace(EntityReference.toXml(transcriber)))
      Site.multi(transcribers)
    })
  )
}
