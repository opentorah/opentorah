package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Body, EntityReference, Page, Tei, Title}
import org.opentorah.util.Files
import org.opentorah.xml.{Antiparser, Element, FromUrl, Parser, Xml}
import java.net.URL

final class Collection(
  override val fromUrl: FromUrl,
  override val names: Names,
  val alias: Option[String],
  val title: Option[Title.Value],
  val storeAbstract: Option[Abstract.Value],
  val body: Option[Body.Value],
  override val directory: String,
  val parts: Seq[CollectionPart]
) extends Directory[Tei, Document](directory, "xml", Document) with Store {

  def pageType: Page.Type = ??? // TODO

  override protected def loadFile(url: URL): Tei = Parser.parseDo(Tei.parse(url))

  private def collectionDocuments: Collection.Documents = Cache.get[Collection.Documents](
    directoryUrl,
    _ /* url */ => new Collection.Documents(readDirectory, parts) // TODO use url?
  )

  override def directoryEntries: Seq[Document] = documents
  def documents: Seq[Document] = collectionDocuments.directoryEntries

  def find(name: String): Option[Document] = collectionDocuments.find(name)

  val htmlFacet     : Collection.HtmlFacet = new Collection.HtmlFacet(this)
  val facsimileFacet: Collection.FacsFacet = new Collection.FacsFacet(this)
  val teiFacet      : Collection.TeiFacet  = new Collection.TeiFacet (this)

  // With no facet, "document" is assumed
  override def findByName(name: String): Option[Store] = htmlFacet.findByName(name)
    .orElse(Store.findByName(name, Seq(htmlFacet, facsimileFacet, teiFacet)))

  def indexContent(path: Store.Path): String = ???

  private def teiBody: Seq[Xml.Node] = {
    val pages: Seq[Page] = ??? // TODO directoryEntries.flatMap(document => document.pages(collection.pageType))

    def listMissing(flavour: String, isMissing: Page => Boolean): Seq[Xml.Element] = {
      val missing: Seq[String] = pages.filter(isMissing).map(_.displayName)
      if (missing.isEmpty) Seq.empty
      else Seq(<p>Отсутствуют фотографии {missing.length} {flavour} страниц: {missing.mkString(" ")}</p>)
    }

// TODO    Hierarchy.storeHeader(collection.path, collection) ++
      Seq[Xml.Element](Collection.table(this).toTei(
        collectionDocuments.parts.flatMap { part =>
          part.title.fold[Seq[Xml.Node]](Seq.empty)(_.xml).map(Table.Nodes) ++
            part.documents.map(Table.Data[Document]) }
      )) ++
      listMissing("пустых", page => page.pb.isMissing && page.pb.isEmpty) ++
      listMissing("непустых", page => page.pb.isMissing && !page.pb.isEmpty)
  }
}

// TODO rename the element to "collection" when switched to the new generation
object Collection extends Element[Collection]("store") {

  private class Documents(
    name2document: Map[String, Document],
    partsRaw: Seq[CollectionPart]
  ) {
    def directoryEntries: Seq[Document] = name2document.values.toSeq

    def find(name: String): Option[Document] = name2document.get(name)

    lazy val parts: Seq[CollectionPart.Part] = CollectionPart.getParts(partsRaw, directoryEntries)

    // TODO document -> translations; document -> prev/next
  }

  sealed abstract class Facet(val collection: Collection) extends By {
    final override def findByName(name: String): Option[Document.Facet] = Store
      .checkExtension(name, extension)
      .flatMap(collection.find)
      .map(of)

    final def of(document: Document): Document.Facet = new Document.Facet(document, this)

    def extension: String
  }

  final class TeiFacet(collection: Collection) extends Facet(collection) {
    override def selector: Selector = Selector.byName("tei")
    override def extension: String = "xml"
  }

  final class HtmlFacet(collection: Collection) extends Facet(collection) {
    override def selector: Selector = Selector.byName("document")
    override def extension: String = "html"
  }

  final class FacsFacet(collection: Collection) extends Facet(collection) {
    override def selector: Selector = Selector.byName("facsimile")
    override def extension: String = "html"
  }

  def table(collection: Collection): Table[Document] = new Table[Document](
    Table.Column("Описание", "description", _.description.map(_.xml).getOrElse(Seq.empty)),
    Table.Column("Дата", "date", document => Xml.mkText(document.date.getOrElse(""))),
    Table.Column("Кто", "author", document => Xml.multi(document.authors.flatMap(_.xml))),
    Table.Column("Кому", "addressee", document =>
      Seq(document.addressee.fold[Xml.Node](Xml.mkText(""))(addressee => EntityReference.toXmlElement(addressee)))),

    // TODO
//    Table.Column("Язык", "language", { document: Document =>
//      val translations: Seq[Xml.Element] =
//        for (teiHolder <- document.by.get.stores.filter(_.language.isDefined))
//          yield Ref.toXml(DocumentObject.documentUrl(collection, teiHolder.name), teiHolder.language.get)
//      val language: Option[String] = document.tei.languages.map(_.ident).headOption
//        .orElse(document.tei.text.lang)
//      Seq(Xml.mkText(language.getOrElse("?"))) ++ translations.flatMap(r => Seq(Xml.mkText(" "), r))
//    }),
//
//    Table.Column("Документ", "document", { document: Document =>
//      Ref.toXml(DocumentObject.documentUrl(collection, document.name), document.name)
//    }),
//
//    Table.Column("Страницы", "pages", { document: Document =>
//      for (page <- document.pages(collection.value.pageType)) yield page.pb.addAttributes(Ref.toXml(
//        target = DocumentObject.pageUrl(collection, document.name, page),
//        text = page.displayName
//      ))
//    }),

    Table.Column("Расшифровка", "transcriber", { document: Document =>
      val transcribers: Seq[Xml.Node] = document.editors
        .filter(_.role.contains("transcriber")).flatMap(_.persName)
        .map(transcriber => EntityReference.toXmlElement(transcriber))
      Xml.multi(transcribers)
    })
  )

  override def parser: Parser[Collection] = for {
    fromUrl <- currentFromUrl
    names <- Names.withDefaultNameParser
    title <- Title.parsable.optional
    storeAbstract <- Abstract.parsable.optional
    body <- Body.parsable.optional
    // TODO when new generation rules and files are modified, stop parsing (and delete) ByDocument,
    // and start parsing 'alias', 'directory' and 'parts' directly
    byDocument <- ByDocument.required
//    alias <- aliasAttribute.optional
//    directory <- Store.directoryAttribute.optional
//    parts <- Part.all
  } yield new Collection(
    fromUrl,
    names,
    alias = Some(Files.fileName(fromUrl.url)),
    title,
    storeAbstract,
    body,
    byDocument.directory, //directory.getOrElse("tei"),
    byDocument.parts //parts
  )

  override def antiparser: Antiparser[Collection] = Antiparser.concat(
    Names.antiparser(_.names),
    Title.parsable.toXmlOption(_.title),
    Abstract.parsable.toXmlOption(_.storeAbstract),
    Body.parsable.toXmlOption(_.body),
    Directory.directoryToXml,
    CollectionPart.toXmlSeq(_.parts)
  )

  // TODO remove when...
  final class ByDocument(
    val directory: String,
    val parts: Seq[CollectionPart]
  )
  object ByDocument extends Element[ByDocument]("by") {
    override def parser: Parser[ByDocument] = for {
      _ /*selector*/ <- By.selector
      directory <- Directory.directory
      parts <- CollectionPart.all
    } yield new ByDocument(
      directory,
      parts
    )
    override def antiparser: Antiparser[ByDocument] = ???
  }
}
