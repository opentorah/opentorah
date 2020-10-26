package org.opentorah.collector

import org.opentorah.store.WithPath
import org.opentorah.tei.{EntityReference, Page, Ref}
import org.opentorah.util.Files
import org.opentorah.xml.Xml
import scala.xml.{Elem, Node}

final class CollectionObject(site: Site, collection: WithPath[Collection]) extends SimpleSiteObject(site) {

  override protected def urlPrefix: Seq[String] = CollectionObject.urlPrefix(collection)

  override def fileName: String = CollectionObject.fileName

  override protected def viewer: Viewer = Viewer.Collection

  override def isWide: Boolean = true

  override protected def navigationLinks: Seq[NavigationLink] =
    Seq(CollectionObject.navigationLink(collection))

  override protected def teiBody: Seq[Node] = {
    val pages: Seq[Page] = collection.value.documents
      .flatMap(document => document.pages(collection.value.pageType))

    def listMissing(flavour: String, isMissing: Page => Boolean): Seq[Elem] = {
      val missing: Seq[String] = pages.filter(isMissing).map(_.displayName)
      if (missing.isEmpty) Seq.empty
      else Seq(<p>Отсутствуют фотографии {missing.length} {flavour} страниц: {missing.mkString(" ")}</p>)
    }

    Hierarchy.storeHeader(collection.path, collection.value) ++
    Seq[Elem](CollectionObject.table(collection).toTei(
      collection.value.parts.flatMap { part =>
          part.title.fold[Seq[Node]](Seq.empty)(_.xml).map(Table.Nodes) ++
          part.documents.map(Table.Data[Document]) }
    )) ++
      listMissing("пустых", page => page.pb.isMissing && page.pb.isEmpty) ++
      listMissing("непустых", page => page.pb.isMissing && !page.pb.isEmpty)
  }

  override def simpleSubObjects: Seq[SimpleSiteObject] = Seq.empty
}

object CollectionObject {

  val fileName: String = "index"

  def urlPrefix(collection: WithPath[Collection]): Seq[String] =
    Seq(CollectionObject.directoryName, Hierarchy.fileName(collection.value))

  // Note: also hard-coded in 'index.xml'!
  val directoryName: String = "collections"

  val documentsDirectoryName: String = "documents" // TEI documents converted to HTML

  val facsDirectoryName: String = "facs" // facsimile viewers

  def navigationLink(collection: WithPath[Collection]): NavigationLink =
    NavigationLink("../index", s"[${Hierarchy.storeName(collection.value)}]", Some(Viewer.Collection))

  // TODO move into CollectionsObject (together with its directory name)
  def resolve(site: Site, parts: Seq[String]): Option[SiteFile] = if (parts.isEmpty) None else
    site.findCollectionByName(parts.head).flatMap { collection =>
      def resolveDocument(f: DocumentObject => SiteFile): Option[SiteFile] =
        DocumentObject.resolve(site, collection, parts.tail.tail).map(f)

      if (parts.tail.isEmpty) Some(new CollectionObject(site, collection).htmlFile)
      else parts.tail.head match {
        case CollectionObject.documentsDirectoryName => resolveDocument(_.htmlFile)
        case CollectionObject.facsDirectoryName      => resolveDocument(_.facsFile)

        case file => if (parts.tail.tail.nonEmpty) None else {
          val (fileName: String, extension: Option[String]) = Files.nameAndExtension(file)
          if (fileName == CollectionObject.fileName)
            SimpleSiteObject.resolve(extension, new CollectionObject(site, collection))
          else
            DocumentObject.resolve(site, collection, parts.tail).map(_.htmlFile)
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
      Seq(Xml.mkText(language.getOrElse("?"))) ++ translations.flatMap(r => Seq(Xml.mkText(" "), r))
    }),

    Table.Column("Документ", "document", { document: Document =>
      Ref.toXml(DocumentObject.documentUrl(collection, document.name), document.name)
    }),

    Table.Column("Страницы", "pages", { document: Document =>
      for (page <- document.pages(collection.value.pageType)) yield page.pb.addAttributes(Ref.toXml(
        target = DocumentObject.pageUrl(collection, document.name, page),
        text = page.displayName
      ))
    }),

    Table.Column("Расшифровка", "transcriber", { document: Document =>
      val transcribers: Seq[Node] = document.tei.titleStmt.editors
        .filter(_.role.contains("transcriber")).flatMap(_.persName)
        .map(transcriber => EntityReference.toXmlElement(transcriber))
      Xml.multi(transcribers)
    })
  )
}
