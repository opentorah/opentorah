package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Body, Pb, Tei, Title}
import org.opentorah.site.{By, Caching, Directory, HtmlContent, Selector, Store}
import org.opentorah.util.Collections
import org.opentorah.xml.{Attribute, Element, Elements, FromUrl, Parsable, Parser, Unparser, Xml}
import zio.ZIO
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

  override protected def loadFile(url: URL): Parser[Tei] = Tei.parse(url)

  def facsimileUrl(site: Site): String = {
    val pathStr = site.store2path(this).map(_.structureName).mkString("/")
    site.facsimilesUrl + pathStr  + "/"
  }

  def siblings(document: Document): Caching.Parser[(Option[Document], Option[Document])] =
    getDirectory.map(_.siblings(document.baseName))

  def translations(document: Document): Caching.Parser[Seq[Document]] =
    getDirectory.map(_.translations.getOrElse(document.baseName, Seq.empty))

  val teiFacet      : Collection.TeiFacet       = new Collection.TeiFacet      (this)
  val textFacet     : Collection.TextFacet      = new Collection.TextFacet     (this)
  val facsimileFacet: Collection.FacsimileFacet = new Collection.FacsimileFacet(this)

  override def acceptsIndexHtml: Boolean = true

  // With no facet, "document" is assumed
  override def findByName(name: String): Caching.Parser[Option[Store]] = textFacet.findByName(name) >>= { // TODO ZIO.someOrElseM?
    case Some(result) => ZIO.some(result)
    case None => Store.findByName(name, Seq(textFacet, facsimileFacet, teiFacet))
  }

  override protected def innerContent(site: Site): Caching.Parser[Xml.Element] =
    for {
      directory <- getDirectory
      columns = Seq[Collection.Column](
        Collection.descriptionColumn,
        Collection.dateColumn,
        Collection.authorsColumn,
        Collection.addresseeColumn,
        languageColumn(site),

        Collection.Column("Документ", "document", { document: Document =>
          ZIO.succeed(textFacet.of(document).a(site)(text = document.baseName))
        }),

        Collection.Column("Страницы", "pages", { document: Document =>
          val text: Document.TextFacet = textFacet.of(document)
          ZIO.succeed(Xml.multi(separator = " ", nodes = document.pages(pageType).map(page =>
            page.pb.addAttributes(text.a(site).setFragment(Pb.pageId(page.pb.n))(text = page.displayName))
          )))
        }),

        Collection.transcribersColumn
      )
      rows <- ZIO.foreach(directory.parts) { part =>
        ZIO.foreach(part.documents) { document =>
          ZIO.foreach(columns) { column =>
            column.value(document).map(value => <td class={column.cssClass}>{value}</td>)
          }.map(cells => <tr>{cells}</tr>)
        }
          .map(documentRows => part.title.fold[Xml.Nodes](Seq.empty)(title =>
            <tr><td colspan={columns.length.toString}><span class="part-title">{title.xml}</span></td></tr>
          ) ++ documentRows)
      }
    } yield {
      val missingPages: Seq[Page] = directory.entries.sortBy(_.name).flatMap(_.pages(pageType)).filter(_.pb.isMissing)

      def listMissing(flavour: String, isMissing: Page => Boolean): Seq[Xml.Element] = {
        val missing: Seq[String] = missingPages.filter(isMissing).map(_.displayName)
        if (missing.isEmpty) Seq.empty
        else Seq(<p>Отсутствуют фотографии {missing.length} {flavour} страниц: {missing.mkString(" ")}</p>)
      }

      <div>
        <table class="collection-index">
          {<tr>{columns.map(column => <th class={column.cssClass}>{column.heading}</th>)}</tr>}
          {rows.flatten}
        </table>

        {listMissing("пустых"  , page => page.pb.isEmpty)}
        {listMissing("непустых", page => !page.pb.isEmpty)}
      </div>
    }

  private val documentHeaderColumns: Seq[Collection.Column] = Seq[Collection.Column](
    Collection.descriptionColumn,
    Collection.dateColumn,
    Collection.authorsColumn,
    Collection.addresseeColumn,
    Collection.transcribersColumn
  )

  def documentHeader(document: Document): Caching.Parser[Xml.Element] =
    ZIO.foreach(documentHeaderColumns) { column =>
      column.value(document).map(value =>
        <tr>
          <td class="heading">{column.heading}</td>
          <td class="value">{value}</td>
        </tr>
      )
    }.map(rows => <table class="document-header">{rows}</table>)

  private def languageColumn(site: Site): Collection.Column =
    Collection.Column("Язык", "language", { document: Document =>
      translations(document).map(documentTranslations =>
        Seq(Xml.mkText(document.lang)) ++ documentTranslations.flatMap(translation =>
          Seq(Xml.mkText(" "), textFacet.of(translation).a(site)(text = translation.lang))))
    })
}

object Collection extends Element[Collection]("collection") {

  final case class Column(
    heading: String,
    cssClass: String,
    value: Document => Caching.Parser[Xml.Nodes]
  )

  val descriptionColumn : Column = Column("Описание"   , "description", document => ZIO.succeed(document.getDescription ))
  val dateColumn        : Column = Column("Дата"       , "date"       , document => ZIO.succeed(document.getDate        ))
  val authorsColumn     : Column = Column("Кто"        , "author"     , document => ZIO.succeed(document.getAuthors     ))
  val addresseeColumn   : Column = Column("Кому"       , "addressee"  , document => ZIO.succeed(document.getAddressee   ))
  val transcribersColumn: Column = Column("Расшифровка", "transcriber", document => ZIO.succeed(document.getTranscribers))

  final class Alias(val collection: Collection) extends Store with HtmlContent[Site] {

    def alias: String = collection.alias.get

    override val names: Names = Names(alias)

    override def acceptsIndexHtml: Boolean = collection.acceptsIndexHtml

    override def findByName(name: String): Caching.Parser[Option[Store]] = collection.findByName(name)
    override def htmlHeadTitle: Option[String] = collection.htmlHeadTitle
    override def htmlBodyTitle: Option[Xml.Nodes] = collection.htmlBodyTitle
    override def content(site: Site): Caching.Parser[Xml.Element] = collection.content(site)
  }

  final class Documents(
    name2entry: Map[String, Document],
    partsRaw: Seq[CollectionPart]
  ) extends Directory.Wrapper[Document](name2entry) {

    lazy val originalDocuments: Seq[Document] = entries
      .filterNot(_.isTranslation)
      .sortBy(_.baseName)

    lazy val translations: Map[String, Seq[Document]] = Collections.mapValues(entries
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

    final override def findByName(name: String): Caching.Parser[Option[DF]] = Store.findByName(
      name,
      extension,
      name => (collection.getDirectory >>= (_.get(name))).map(_.map(of)),
      // Document name can have dots (e.g., 273.2), so if it is referenced without the extension -
      // assume the required extension is implied, and the one found is part of the document name.
      assumeAllowedExtension = true
    )

    def of(document: Document): DF

    protected def extension: String
  }

  final class TeiFacet(collection: Collection) extends Facet[Document.TeiFacet, TeiFacet](collection) {
    override def selector: Selector = Selector.byName("tei")
    override protected def extension: String = "xml"
    override def of(document: Document): Document.TeiFacet = new Document.TeiFacet(document, this)
  }

  sealed abstract class HtmlFacet[DF <: Document.HtmlFacet[DF, F], F <: HtmlFacet[DF, F]](collection: Collection)
    extends Facet[DF, F](collection)

  final class TextFacet(collection: Collection) extends HtmlFacet[Document.TextFacet, TextFacet](collection) {
    override def selector: Selector = Selector.byName("document")
    override protected def extension: String = "html"
    override def of(document: Document): Document.TextFacet = new Document.TextFacet(document, this)
  }

  final class FacsimileFacet(collection: Collection) extends HtmlFacet[Document.FacsimileFacet, FacsimileFacet](collection) {
    override def selector: Selector = Selector.byName("facsimile")
    override protected def extension: String = "html"
    override def of(document: Document): Document.FacsimileFacet = new Document.FacsimileFacet(document, this)
  }

  override def contentParsable: Parsable[Collection] = new Parsable[Collection] {
    private val namesParsable: Parsable[Names] = Names.withDefaultNameParsable
    private val titleElement: Elements.Required[Title.Value] = Title.element.required
    private val abstractElement: Elements.Optional[Abstract.Value] = Abstract.element.optional
    private val bodyElement: Elements.Optional[Body.Value] = Body.element.optional
    private val aliasAttribute: Attribute.Optional[String] = Attribute("alias").optional
    private val directoryAttribute: Attribute.OrDefault[String] = Attribute("directory", default = "tei").orDefault

    override def parser: Parser[Collection] = for {
      pageType <- Page.typeAttribute()
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
      pageType,
      alias,
      title,
      storeAbstract,
      body,
      directory,
      parts
    )

    override def unparser: Unparser[Collection] = Unparser.concat(
      Page.typeAttribute(_.pageType),
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
