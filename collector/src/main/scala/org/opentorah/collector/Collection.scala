package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Body, Pb, Tei, Title}
import org.opentorah.site.HtmlContent
import org.opentorah.store.{By, Caching, Selector, Store, Stores}
import org.opentorah.xml.{Attribute, Element, Elements, FromUrl, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO

final class Collection(
  fromUrl: FromUrl,
  override val names: Names,
  val pageType: Page.Type,
  val alias: Option[String],
  override val title: Title.Value,
  override val storeAbstract: Option[Abstract.Value],
  override val body: Option[Body.Value],
  val directory: String,
  val parts: Seq[CollectionPart]
) extends Hierarchical {

  val documents: Documents = new Documents(
    fromUrl,
    directory,
    parts
  )

  override def getBy: Option[ByHierarchy] = None

  def facsimileUrl(collector: Collector): String = {
    val pathStr: String = collector.store2path(this).map(_.structureName).mkString("/")
    collector.common.getTei.facsimilesUrl.getOrElse("/") + pathStr  + "/"
  }

  def siblings(document: Document): Caching.Parser[(Option[Document], Option[Document])] =
    documents.getDirectory.map(_.siblings(document.baseName))

  def translations(document: Document): Caching.Parser[Seq[Document]] =
    documents.getDirectory.map(_.translations.getOrElse(document.baseName, Seq.empty))

  val textFacet     : Collection.TextFacet      = new Collection.TextFacet     (this)
  val facsimileFacet: Collection.FacsimileFacet = new Collection.FacsimileFacet(this)

  override def storesPure: Seq[Store/*TODO Collection.Facet[_,  _]*/] =
    Seq(textFacet, facsimileFacet)

  // With no facet, "document" is assumed
  override def findByName(name: String): Caching.Parser[Option[Store]] =
    textFacet.findByName(name).flatMap {
      case Some(result) => ZIO.some(result) // TODO ZIO.someOrElseM?
      case None => super.findByName(name)
    }

  override protected def innerContent(collector: Collector): Caching.Parser[ScalaXml.Element] =
    for {
      directory <- documents.getDirectory
      columns = Seq[Collection.Column](
        Collection.descriptionColumn,
        Collection.dateColumn,
        Collection.authorsColumn,
        Collection.addresseeColumn,
        languageColumn(collector),

        Collection.Column("Документ", "document", { (document: Document) =>
          ZIO.succeed(textFacet.of(document).a(collector)(text = document.baseName))
        }),

        Collection.Column("Страницы", "pages", { (document: Document) =>
          val text: Document.TextFacet = textFacet.of(document)
          ZIO.succeed(ScalaXml.multi(separator = " ", nodes = document.pages(pageType).map(page =>
            page.pb.addAttributes(text.a(collector).setFragment(Pb.pageId(page.pb.n))(text = page.displayName))
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
          .map(documentRows => part.title.fold[ScalaXml.Nodes](Seq.empty)(title =>
            <tr><td colspan={columns.length.toString}><span class="part-title">{title.content}</span></td></tr>
          ) ++ documentRows)
      }
    } yield {
      val missingPages: Seq[Page] = directory.stores.sortBy(_.name).flatMap(_.pages(pageType)).filter(_.pb.isMissing)

      def listMissing(flavour: String, isMissing: Page => Boolean): Seq[ScalaXml.Element] = {
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

  def documentHeader(document: Document): Caching.Parser[ScalaXml.Element] =
    ZIO.foreach(documentHeaderColumns) { column =>
      column.value(document).map(value =>
        <tr>
          <td class="heading">{column.heading}</td>
          <td class="value">{value}</td>
        </tr>
      )
    }.map(rows => <table class="document-header">{rows}</table>)

  private def languageColumn(collector: Collector): Collection.Column =
    Collection.Column("Язык", "language", { (document: Document) =>
      translations(document).map(documentTranslations =>
        Seq(ScalaXml.mkText(document.lang)) ++ documentTranslations.flatMap(translation =>
          Seq(ScalaXml.mkText(" "), textFacet.of(translation).a(collector)(text = translation.lang))))
    })
}

object Collection extends Element[Collection]("collection") {

  final case class Column(
    heading: String,
    cssClass: String,
    value: Document => Caching.Parser[ScalaXml.Nodes]
  )

  val descriptionColumn : Column = Column("Описание"   , "description", document => ZIO.succeed(document.getDescription ))
  val dateColumn        : Column = Column("Дата"       , "date"       , document => ZIO.succeed(document.getDate        ))
  val authorsColumn     : Column = Column("Кто"        , "author"     , document => ZIO.succeed(document.getAuthors     ))
  val addresseeColumn   : Column = Column("Кому"       , "addressee"  , document => ZIO.succeed(document.getAddressee   ))
  val transcribersColumn: Column = Column("Расшифровка", "transcriber", document => ZIO.succeed(document.getTranscribers))

  final class Alias(val collection: Collection) extends Store.NonTerminal with Stores.Pure with HtmlContent[Collector] {
    def alias: String = collection.alias.get

    override val names: Names = Names(alias)

    override def findByName(name: String): Caching.Parser[Option[Store]] = collection.findByName(name)
    override def storesPure: Seq[Store] = collection.storesPure
    override def htmlHeadTitle: Option[String] = collection.htmlHeadTitle
    override def htmlBodyTitle: Option[ScalaXml.Nodes] = collection.htmlBodyTitle
    override def content(collector: Collector): Caching.Parser[ScalaXml.Element] = collection.content(collector)
  }

  sealed abstract class Facet[DF <: Document.Facet](val collection: Collection) extends By {

    final override def findByName(name: String): Caching.Parser[Option[DF]] =
      collection.documents.findByName(name).map(_.map(of))

    override def stores: Caching.Parser[Seq[Store]] =
      collection.documents.stores.map(_.map(of))

    final def getTei(document: Document): Caching.Parser[Tei] =
      collection.documents.getFile(document)

    def of(document: Document): DF
  }

  final class TextFacet(collection: Collection) extends Facet[Document.TextFacet](collection) {
    override def selector: Selector = Selector.byName("document")
    override def of(document: Document): Document.TextFacet = new Document.TextFacet(document, this)
  }

  final class FacsimileFacet(collection: Collection) extends Facet[Document.FacsimileFacet](collection) {
    override def selector: Selector = Selector.byName("facsimile")
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
