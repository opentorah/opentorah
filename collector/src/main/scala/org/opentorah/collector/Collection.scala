package org.opentorah.collector

import org.opentorah.html
import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Body, Pb, Tei, Title}
import org.opentorah.site.HtmlContent
import org.opentorah.store.{By, Path, Selector, Store}
import org.opentorah.xml.{Attribute, Caching, Element, Elements, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO

final class Collection(
  fromUrl: Element.FromUrl,
  names: Names,
  title: Title.Value,
  storeAbstract: Option[Abstract.Value],
  body: Option[Body.Value],
  val pageType: Page.Type,
  val directory: String,
  val parts: Seq[CollectionPart]
) extends Hierarchical(
  fromUrl,
  names,
  title,
  storeAbstract,
  body
),
HtmlContent.Wide derives CanEqual:

  override def equals(other: Any): Boolean =
    val that: Collection = other.asInstanceOf[Collection]
    this.directory == that.directory

  val documents: Documents = Documents(
    this,
    fromUrl,
    directory,
    parts
  )

  override def getBy: Option[ByHierarchy] = None

  def siblings(document: Document): Caching.Parser[(Option[Document], Option[Document])] =
    documents.getDirectory.map(_.siblings(document.baseName))

  def translations(document: Document): Caching.Parser[Seq[Document]] =
    documents.getDirectory.map(_.translations.getOrElse(document.baseName, Seq.empty))

  val textFacet     : Collection.CollectionTextFacet      = Collection.CollectionTextFacet     (this)
  val facsimileFacet: Collection.CollectionFacsimileFacet = Collection.CollectionFacsimileFacet(this)

  override def storesPure: Seq[Collection.CollectionFacet[?]] =
    Seq(textFacet, facsimileFacet)

  // With no facet, "document" is assumed
  override def findByName(name: String): Caching.Parser[Option[Store]] =
    textFacet.findByName(name).flatMap(_.fold(super.findByName(name))(ZIO.some)) // TODO ZIO.someOrElseM?

  override protected def innerContent(path: Path, collector: Collector): Caching.Parser[ScalaXml.Element] = for
    directory: Documents.All <- documents.getDirectory
    columns: Seq[Collection.Column] = Seq(
      Collection.descriptionColumn,
      Collection.dateColumn,
      Collection.authorsColumn,
      Collection.addresseeColumn,

      Collection.Column("Язык", "language", (document: Document) =>
        translations(document).map(documentTranslations =>
          Seq(ScalaXml.mkText(document.lang)) ++ documentTranslations.flatMap(translation =>
            Seq(ScalaXml.mkText(" "), translation.textFacetLink(path, collector)(text = translation.lang))
          )
        )
      ),

      Collection.Column("Документ", "document", (document: Document) =>
        ZIO.succeed(document.textFacetLink(path, collector)(text = document.baseName))
      ),

      Collection.Column("Страницы", "pages", (document: Document) =>
        ZIO.succeed(ScalaXml.multi(separator = " ", nodes = for page <- document.pages(pageType)
          yield page.pb.addAttributes(document.textFacetLink(path, collector).setFragment(Pb.pageId(page.pb.n))(text = page.displayName))))
      ),

      Collection.transcribersColumn
    )

    rows: Seq[ScalaXml.Nodes] <- ZIO.foreach(directory.parts)(part =>
      ZIO.foreach(part.documents)(document =>
        ZIO.foreach(columns)(column =>
          column.value(document).map(value => <td class={column.cssClass}>{value}</td>)
        ).map(cells => <tr>{cells}</tr>)
      )
        .map(documentRows => part.title.fold[ScalaXml.Nodes](Seq.empty)(title =>
          <tr><td colspan={columns.length.toString}><span class="part-title">{title.content.scalaXml}</span></td></tr>
        ) ++ documentRows)
    )
  yield
    val missingPages: Seq[Page] = directory.stores.sortBy(_.name).flatMap(_.pages(pageType)).filter(_.pb.isMissing)

    def listMissing(flavour: String, isMissing: Page => Boolean): Seq[ScalaXml.Element] =
      val missing: Seq[String] = missingPages.filter(isMissing).map(_.displayName)
      if missing.isEmpty then Seq.empty
      else Seq(<p>Отсутствуют фотографии {missing.length} {flavour} страниц: {missing.mkString(" ")}</p>)

    <div>
      <table class="collection-index">
        {<tr>{for column <- columns yield <th class={column.cssClass}>{column.heading}</th>}</tr>}
        {rows.flatten}
      </table>

      {listMissing("пустых"  , page => page.pb.isEmpty)}
      {listMissing("непустых", page => !page.pb.isEmpty)}
    </div>

  private val documentHeaderColumns: Seq[Collection.Column] = Seq[Collection.Column](
    Collection.descriptionColumn,
    Collection.dateColumn,
    Collection.authorsColumn,
    Collection.addresseeColumn,
    Collection.transcribersColumn
  )

  def documentHeader(document: Document): Caching.Parser[ScalaXml.Element] =
    ZIO.foreach(documentHeaderColumns)(column =>
      column.value(document).map(value =>
        <tr>
          <td class="heading">{column.heading}</td>
          <td class="value">{value}</td>
        </tr>
      )
    ).map(rows => <table class="document-header">{rows}</table>)

object Collection extends Element[Collection]("collection"):

  final class Column(
    val heading: String,
    val cssClass: String,
    val value: Document => Caching.Parser[ScalaXml.Nodes]
  ):
    override def toString: String = heading

  val descriptionColumn : Column = Column("Описание"   , "description", document => ZIO.succeed(document.getDescription ))
  val dateColumn        : Column = Column("Дата"       , "date"       , document => ZIO.succeed(document.getDate        ))
  val authorsColumn     : Column = Column("Кто"        , "author"     , document => ZIO.succeed(document.getAuthors     ))
  val addresseeColumn   : Column = Column("Кому"       , "addressee"  , document => ZIO.succeed(document.getAddressee   ))
  val transcribersColumn: Column = Column("Расшифровка", "transcriber", document => ZIO.succeed(document.getTranscribers))

  // TODO drop DF?
  sealed abstract class CollectionFacet[DF <: Facet](val collection: Collection) extends By[DF]:

    final override def findByName(name: String): Caching.Parser[Option[DF]] =
      collection.documents.findByName(name).map(_.map(of))

    override def stores: Caching.Parser[Seq[DF]] =
      collection.documents.stores.map(_.map(of))

    final def getTei(document: Document): Caching.Parser[Tei] =
      collection.documents.getFile(document)

    def of(document: Document): DF

  final class CollectionTextFacet(collection: Collection) extends CollectionFacet[TextFacet](collection):
    override def selector: Selector = Selector.getForName("document")
    override def of(document: Document): TextFacet = TextFacet(document, this)

  final class CollectionFacsimileFacet(collection: Collection) extends CollectionFacet[FacsimileFacet](collection):
    override def selector: Selector = Selector.getForName("facsimile")
    override def of(document: Document): FacsimileFacet = FacsimileFacet(document, this)

  override def contentParsable: Parsable[Collection] = new Parsable[Collection]:
    private val namesParsable: Parsable[Names] = Names.withDefaultNameParsable
    private val titleElement: Elements.Required[Title.Value] = Title.element.required
    private val abstractElement: Elements.Optional[Abstract.Value] = Abstract.element.optional
    private val bodyElement: Elements.Optional[Body.Value] = Body.element.optional
    private val directoryAttribute: Attribute.OrDefault[String] = Attribute("directory", default = "tei").orDefault

    override def parser: Parser[Collection] = for
      fromUrl: Element.FromUrl <- Element.fromUrl
      names: Names <- namesParsable()
      title: Title.Value <- titleElement()
      storeAbstract: Option[Abstract.Value] <- abstractElement()
      body: Option[Body.Value] <- bodyElement()
      pageType: Page.Type <- Page.typeAttribute()
      directory: String <- directoryAttribute()
      parts: Seq[CollectionPart] <- CollectionPart.seq()
    yield Collection(
      fromUrl,
      names,
      title,
      storeAbstract,
      body,
      pageType,
      directory,
      parts
    )

    override def unparser: Unparser[Collection] = Unparser.concat(
      namesParsable(_.names),
      titleElement(_.title),
      abstractElement(_.storeAbstract),
      bodyElement(_.body),
      Page.typeAttribute(_.pageType),
      directoryAttribute(_.directory),
      CollectionPart.seq(_.parts)
    )
