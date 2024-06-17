package org.opentorah.collector

import org.opentorah.html.A
import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Body, Title}
import org.opentorah.store.{Context, Path, Selector, Store}
import org.opentorah.xml.{Atom, Attribute, Element, ElementTo, Elements, FromUrl, Nodes, Parsable, Parser, Unparser}
import zio.ZIO

final class Collection(
  fromUrl: FromUrl,
  names: Names,
  title: Title.Value,
  description: Option[Abstract.Value],
  body: Option[Body.Value],
  val pageType: Page.Type,
  val directory: String,
  val parts: Seq[CollectionPart]
) extends Hierarchical(
  fromUrl,
  names,
  title,
  description,
  body
) derives CanEqual:

  override def toString: String = s"Collection $names $directory [${fromUrl.url}]"

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

  def siblings(document: Document): Parser[(Option[Document], Option[Document])] =
    documents.getDirectory.map(_.siblings(document.baseName))

  def translations(document: Document): Parser[Seq[Document]] =
    documents.getDirectory.map(_.translations.getOrElse(document.baseName, Seq.empty))

  val textFacet: CollectionFacet = new CollectionFacet(this):
    override def selector: Selector = Selector.getForName("document")
    override def of(document: Document): TextFacet = TextFacet(document, this)
    override def isText: Boolean = true

  val facsimileFacet: CollectionFacet = new CollectionFacet(this):
    override def selector: Selector = Selector.getForName("facsimile")
    override def of(document: Document): FacsimileFacet = FacsimileFacet(document, this)
    override def isText: Boolean = false

  override def storesPure: Seq[CollectionFacet] = Seq(textFacet, facsimileFacet)

  // With no facet, "document" is assumed
  override def findByName(name: String): Parser[Option[Store]] =
    textFacet.findByName(name).flatMap(_.fold(super.findByName(name))(ZIO.some)) // TODO ZIO.someOrElseM?

  override def content(path: Path, context: Context): Parser[Element] = for
    directory: Documents.All <- documents.getDirectory
    columns: Seq[Collection.Column] = Seq(
      Collection.descriptionColumn,
      Collection.dateColumn,
      Collection.authorsColumn,
      Collection.addresseeColumn,

      Collection.Column("Язык", "language", (document: Document) =>
        for
          documentTranslations: Seq[Document] <- translations(document)
          translationLinks <- ZIO.foreach(documentTranslations)((translation: Document) =>
            for textFacetA: A <- translation.textFacetLink(context, path)
            yield Seq(Atom(" "), textFacetA(text = translation.lang)))
        yield
          Seq(Atom(document.lang)) ++ translationLinks.flatten
      ),

      Collection.Column("Документ", "document", (document: Document) =>
        for textFacetA: A <- document.textFacetLink(context, path)
        yield textFacetA(text = document.baseName)
      ),

      Collection.Column("Страницы", "pages", (document: Document) =>
        for
          nodes <- ZIO.foreach(document.pages(pageType))((page: Page) =>
            page.reference(context, document, path)
          )
        yield
          Nodes.multi(separator = " ", nodes = nodes)
      ),

      Collection.transcribersColumn
    )

    rows: Seq[Nodes] <- ZIO.foreach(directory.parts)(part =>
      ZIO.foreach(part.documents)(document =>
        for
          cells <- ZIO.foreach(columns)(column =>
            for value: Nodes <- column.value(document)
            yield <td class={column.cssClass}>{value}</td>
          )
        yield <tr>{cells}</tr>
      )
        .map(documentRows => part.title.fold[Nodes](Seq.empty)(title =>
          <tr><td colspan={columns.length.toString}><span class="part-title">{title.content}</span></td></tr>
        ) ++ documentRows)
    )
  yield
    val missingPages: Seq[Page] = directory.stores.sortBy(_.name).flatMap(_.pages(pageType)).filter(_.pb.isMissing)

    def listMissing(flavour: String, isMissing: Page => Boolean): Elements =
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

  def documentHeader(document: Document): Parser[Element] =
    ZIO.foreach(documentHeaderColumns)(column => column.value(document).map(value =>
      <tr>
        <td class="heading">{column.heading}</td>
        <td class="value">{value}</td>
      </tr>
    )).map(rows => <table class="document-header">{rows}</table>)

object Collection extends ElementTo[Collection]("collection"):

  private class Column(
    val heading: String,
    val cssClass: String,
    val value: Document => Parser[Nodes]
  ):
    override def toString: String = heading

  private class PureColumn(
    heading: String,
    cssClass: String,
    value: Document => Nodes
  ) extends Column(
    heading,
    cssClass,
    document => ZIO.succeed(value(document))
  )

  private val descriptionColumn : Column = PureColumn("Описание"   , "description", _.getDescription )
  private val dateColumn        : Column = PureColumn("Дата"       , "date"       , _.getDate        )
  private val authorsColumn     : Column = PureColumn("Кто"        , "author"     , _.getAuthors     )
  private val addresseeColumn   : Column = PureColumn("Кому"       , "addressee"  , _.getAddressee   )
  private val transcribersColumn: Column = PureColumn("Расшифровка", "transcriber", _.getTranscribers)

  override def contentParsable: Parsable[Collection] = new Parsable[Collection]:
    private val directoryAttribute: Attribute.OrDefault[String] = Attribute("directory", default = "tei").orDefault

    override def parser: Parser[Collection] = for
      fromUrl: FromUrl <- FromUrl.get
      names: Names <- Hierarchical.namesParsable()
      title: Title.Value <- Hierarchical.titleElement()
      description: Option[Abstract.Value] <- Hierarchical.descriptionElement()
      body: Option[Body.Value] <- Hierarchical.bodyElement()
      pageType: Page.Type <- Page.typeAttribute()
      directory: String <- directoryAttribute()
      parts: Seq[CollectionPart] <- CollectionPart.seq()
    yield Collection(
      fromUrl,
      names,
      title,
      description,
      body,
      pageType,
      directory,
      parts
    )

    override def unparser: Unparser[Collection] = Unparser.concat(
      Hierarchical.namesParsable(_.names),
      Hierarchical.titleElement(_.title),
      Hierarchical.descriptionElement(_.description),
      Hierarchical.bodyElement(_.body),
      Page.typeAttribute(_.pageType),
      directoryAttribute(_.directory),
      CollectionPart.seq(_.parts)
    )
