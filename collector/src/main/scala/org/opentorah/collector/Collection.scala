package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Body, Pb, Tei, Title}
import org.opentorah.util.Collections
import org.opentorah.xml.{Attribute, Element, Elements, FromUrl, Html, Parsable, Parser, Unparser, Xml}
import java.net.URL

final class Collection(
  override val fromUrl: FromUrl,
  override val names: Names,
  val pageType: Page.Type,
  val alias: Option[String],
  override val title: Title.Value,
  override val storeAbstract: Option[Abstract.Value],
  override val body: Option[Body.Value],
  override val directory: String,
  val parts: Seq[CollectionPart]
) extends Directory[Tei, Document, Collection.Documents](
  directory,
  "xml",
  Document,
  new Collection.Documents(_, parts)
) with Hierarchical {

  override def getBy: Option[ByHierarchy] = None

  override protected def loadFile(url: URL): Tei = Parser.parseDo(Tei.parse(url))

  override def directoryEntries: Seq[Document] = documents

  private def collectionDocuments: Collection.Documents = getDirectory

  def documents: Seq[Document] =
    collectionDocuments.documents

  def siblings(document: Document): (Option[Document], Option[Document]) =
    collectionDocuments.siblings(document.baseName)

  def translations(document: Document): Seq[Document] =
    collectionDocuments.translations.getOrElse(document.baseName, Seq.empty)

  private def findDocument(name: String): Option[Document] =
    collectionDocuments.name2document.get(name)

  def original(document: Document): Document =
    if (!document.isTranslation) document
    else findDocument(document.baseName).get

  val teiFacet      : Collection.TeiFacet       = new Collection.TeiFacet      (this)
  val textFacet     : Collection.TextFacet      = new Collection.TextFacet     (this)
  val facsimileFacet: Collection.FacsimileFacet = new Collection.FacsimileFacet(this)

  override def acceptsIndexHtml: Boolean = true

  // With no facet, "document" is assumed
  override def findByName(name: String): Option[Store] = textFacet.findByName(name)
    .orElse(Store.findByName(name, Seq(textFacet, facsimileFacet, teiFacet)))

  override def viewer: Viewer = Viewer.Collection
  override def isWide: Boolean = true

  override def path(site: Site): Store.Path =
    alias.fold(site.store2path(this))(alias => Seq(site.alias2collectionAlias(alias)))

  override def navigationLinks(site: Site): Seq[Xml.Element] = Seq(
    a(site)(s"[${names.name}]")
  )

  override protected def innerContent(site: Site): Xml.Element = {
    val missingPages: Seq[Page] = documents.flatMap(_.pages(pageType)).filter(_.pb.isMissing)

    def listMissing(flavour: String, isMissing: Page => Boolean): Seq[Xml.Element] = {
      val missing: Seq[String] = missingPages.filter(isMissing).map(_.displayName)
      if (missing.isEmpty) Seq.empty
      else Seq(<p>Отсутствуют фотографии {missing.length} {flavour} страниц: {missing.mkString(" ")}</p>)
    }

    final case class Column(
      heading: String,
      cssClass: String,
      value: Document => Seq[Xml.Node]
    )

    val columns: Seq[Column] = Seq[Column](
      Column("Описание", "description", _.getDescription),
      Column("Дата", "date", _.getDate),
      Column("Кто", "author", _.getAuthors),
      Column("Кому", "addressee", _.getAddressee),

      Column("Язык", "language", { document: Document =>
        Seq(Xml.mkText(document.lang)) ++ translations(document).flatMap(translation =>
          Seq(Xml.mkText(" "), textFacet.of(translation).a(site)(text = translation.lang)))
      }),

      Column("Документ", "document", { document: Document =>
        textFacet.of(document).a(site)(text = document.baseName)
      }),

      Column("Страницы", "pages", { document: Document =>
        val text: Document.TextFacet = textFacet.of(document)
        for (page <- document.pages(pageType)) yield page.pb.addAttributes(
          text.a(site, part = Some(Pb.pageId(page.pb.n)))(text = page.displayName)
        )
      }),

      Column("Расшифровка", "transcriber", _.getTranscribers)
    )

    <div>
      <table class="collection-index">
        {<tr>{for (column <- columns) yield <th class={column.cssClass}>{column.heading}</th>}</tr>}
        {collectionDocuments.parts.flatMap { part =>
          part.title.fold[Seq[Xml.Node]](Seq.empty)(title =>
            <tr><td colspan={columns.length.toString}><span class="part-title">{title.xml}</span></td></tr>
          ) ++
          part.documents.map(data =>
            <tr>{for (column <- columns) yield <td class={column.cssClass}>{column.value(data)}</td>}</tr>
          )
        }}
      </table>

      {listMissing("пустых"  , page => page.pb.isEmpty)}
      {listMissing("непустых", page => !page.pb.isEmpty)}
    </div>
  }
}

object Collection extends Element[Collection]("collection") {

  final class Alias(val collection: Collection) extends Store with HtmlContent {

    def alias: String = collection.alias.get

    override val names: Names = Names(alias)

    override def findByName(name: String): Option[Store] = collection.findByName(name)

    override def viewer: Viewer = collection.viewer
    override def isWide: Boolean = collection.isWide
    override def htmlHeadTitle: Option[String] = collection.htmlHeadTitle
    override def htmlBodyTitle: Option[Seq[Xml.Node]] = collection.htmlBodyTitle
    override def lang: Option[String] = collection.lang
    override def path           (site: Site): Store.Path       = collection.path           (site)
    override def navigationLinks(site: Site): Seq[Xml.Element] = collection.navigationLinks(site)
    override def content        (site: Site): Xml.Element      = collection.content        (site)
  }

  final class Documents(
    val name2document: Map[String, Document],
    partsRaw: Seq[CollectionPart]
  ) {
    lazy val documents: Seq[Document] = name2document.values.toSeq

    lazy val originalDocuments: Seq[Document] = documents
      .filterNot(_.isTranslation)
      .sortBy(_.baseName)

    lazy val translations: Map[String, Seq[Document]] = Collections.mapValues(documents
      .filter(_.isTranslation)
      .map(document => (document.baseName, document))
      .groupBy(_._1))(_.map(_._2))

    lazy val siblings: Map[String, (Option[Document], Option[Document])] =
      Collections.prevAndNext(originalDocuments)
        .map { case (document, siblings) => (document.baseName, siblings)}
        .toMap

    lazy val parts: Seq[CollectionPart.Part] = CollectionPart.getParts(partsRaw, originalDocuments)
  }

  sealed abstract class Facet[DF <: Document.Facet[DF, F], F <: Facet[DF, F]](val collection: Collection) extends By {
    final override def findByName(name: String): Option[DF] =
      Store.checkExtension(name, extension, assumeAllowedExtension)
      .flatMap(collection.findDocument)
      .map(of)

    def of(document: Document): DF

    def extension: String

    protected def assumeAllowedExtension: Boolean = false
  }

  final class TeiFacet(collection: Collection) extends Facet[Document.TeiFacet, TeiFacet](collection) {
    override def selector: Selector = Selector.byName("tei")
    override def extension: String = "xml"
    override def of(document: Document): Document.TeiFacet = new Document.TeiFacet(document, this)
  }

  sealed abstract class HtmlFacet[DF <: Document.HtmlFacet[DF, F], F <: HtmlFacet[DF, F]](collection: Collection)
    extends Facet[DF, F](collection)

  final class TextFacet(collection: Collection) extends HtmlFacet[Document.TextFacet, TextFacet](collection) {
    override def selector: Selector = Selector.byName("document")
    override def extension: String = "html"
    override def of(document: Document): Document.TextFacet = new Document.TextFacet(document, this)

    // Document name can have dots (e.g., 273.2), so if it is referenced without the extension -
    // assume the required extension is implied, and the one found is part of the document name.
    override protected def assumeAllowedExtension: Boolean = true
  }

  final class FacsimileFacet(collection: Collection) extends HtmlFacet[Document.FacsimileFacet, FacsimileFacet](collection) {
    override def selector: Selector = Selector.byName("facsimile")
    override def extension: String = "html"
    override def of(document: Document): Document.FacsimileFacet = new Document.FacsimileFacet(document, this)
  }

  override def contentParsable: Parsable[Collection] = new Parsable[Collection] {
    private val namesParsable: Parsable[Names] = Names.withDefaultNameParsable
    private val titleElement: Elements.Required[Title.Value] = Title.element.required
    private val abstractElement: Elements.Optional[Abstract.Value] = Abstract.element.optional
    private val bodyElement: Elements.Optional[Body.Value] = Body.element.optional
    private val pageTypeAttribute: Attribute.OrDefault[String] = Attribute("pageType", default = "manuscript").orDefault
    private val aliasAttribute: Attribute.Optional[String] = Attribute("alias").optional
    private val directoryAttribute: Attribute.OrDefault[String] = Attribute("directory", default = "tei").orDefault

    override def parser: Parser[Collection] = for {
      pageType <- pageTypeAttribute()
      fromUrl <- Element.currentFromUrl
      names <- namesParsable()
      title <- titleElement()
      storeAbstract <- abstractElement()
      body <- bodyElement()
      alias <- aliasAttribute()
      directory <- directoryAttribute()
      parts <- CollectionPart.seq()
    } yield new Collection(
      fromUrl,
      names,
      Page.values.find(_.name == pageType).get,
      alias,
      title,
      storeAbstract,
      body,
      directory,
      parts
    )

    override def unparser: Unparser[Collection] = Unparser.concat(
      pageTypeAttribute(_.pageType.name),
      namesParsable(_.names),
      titleElement(_.title),
      abstractElement(_.storeAbstract),
      bodyElement(_.body),
      aliasAttribute(_.alias),
      directoryAttribute(_.directory),
      CollectionPart.seq(_.parts)
    )
  }
}
