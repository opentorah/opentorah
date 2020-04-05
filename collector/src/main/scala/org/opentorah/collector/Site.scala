package org.opentorah.collector

import java.io.File
import org.opentorah.entity.{EntitiesList, Entity, EntityName, EntityReference}
import org.opentorah.metadata.{Language, Names}
import org.opentorah.store.{Binding, By, Entities, EntityHolder, Path, Store, WithPath}
import org.opentorah.tei.Tei
import org.opentorah.util.{Collections, Files}
import org.opentorah.xml.{RawXml, XmlUtil}
import scala.xml.{Elem, Node}

final class Site(store: Store) {
  val stores: Seq[WithPath[Store]] = store.withPath[Store](values = {
    case _: Collection | _: Document | _: Entities | _: EntityHolder | _: TeiHolder => Seq.empty
    case store => Seq(store)
  })

  val collections: Seq[WithPath[Collection]] = store.withPath[Collection](values = {
    case collection: Collection => Seq(collection)
    case _ => Seq.empty
  })

  val entities: Seq[Entity] = store.entities.get.by.get.stores.map(_.entity)

  val entitiesLists: Seq[EntitiesList] = store.entities.get.lists.filterNot(_.isEmpty)

  def resolve(url: String): Option[Site.SiteFile] = {
    if (!url.startsWith("/")) None else {
      val parts: Seq[String] = Site.removePart(url).substring(1).split("/")
      if (parts.isEmpty) None /* TODO site index... */ else {
        val tail: Seq[String] = parts.tail
        parts.head match {
          case Site.hierarchyDirectoryName => resolveHierarchy(Path.empty, store, tail)
          case Site.collectionsDirectoryName => resolveCollection(tail)
          case Site.namesDirectoryName => resolveEntity(tail)
          case file if file.startsWith(Site.namesFileName) =>
            resolveNamesFile(file.substring(Site.namesFileName.length), tail)
          case _ => None
        }
      }
    }
  }

  private def resolveHierarchy(path: Path, store: Store, parts: Seq[String]): Option[Site.SiteFile] =
    if (parts.isEmpty) Some(Site.SiteFile.HierarchyFile(store, Site.TeiFacet.Html)) else parts.head match {
      case "index.html" => Some(Site.SiteFile.HierarchyFile(store, Site.TeiFacet.Html))
      case "index.xml" => Some(Site.SiteFile.HierarchyFile(store, Site.TeiFacet.Xml))
      case _ =>
        val selector = store.by.get.selector
        val selectorName: String = parts.head
        if (selector.names.find(selectorName).isEmpty) None else store match {
          case collection: Collection =>
            if (parts.tail.nonEmpty) None
            else Some(Site.SiteFile.CollectionIndex(WithPath(path, collection), Site.TeiFacet.Html))

          case _ => if (parts.tail.isEmpty) None else {
            val storeName: String = parts.tail.head.replace('_', ' ')
            store.by.get.stores.find(_.names.find(storeName).isDefined).flatMap { nextStore =>
              resolveHierarchy(path :+ selector.bind(nextStore), nextStore, parts.tail.tail)
            }
          }
        }
    }

  def resolveCollection(parts: Seq[String]): Option[Site.SiteFile] = if (parts.isEmpty) None else {
    val collectionName: String = parts.head
    collections.find(collection => Site.fileName(collection.value) == collectionName).flatMap { collection =>
      if (parts.tail.isEmpty) Some(Site.SiteFile.CollectionIndex(collection, Site.TeiFacet.Html))
      else parts.tail.head match {
        case "index.html" =>
          if (parts.tail.tail.nonEmpty) None
          else Some(Site.SiteFile.CollectionIndex(collection, Site.TeiFacet.Html))
        case "index.xml" =>
          if (parts.tail.tail.nonEmpty) None
          else Some(Site.SiteFile.CollectionIndex(collection, Site.TeiFacet.Xml))

        case Site.documentsDirectoryName => resolveDocument(collection, parts.tail.tail, Site.DocumentFacet.Document)
        case Site.teiDirectoryName => resolveDocument(collection, parts.tail.tail, Site.DocumentFacet.Tei)
        case Site.facsDirectoryName => resolveDocument(collection, parts.tail.tail, Site.DocumentFacet.Facs)

        case _ => None
      }
    }
  }

  def resolveDocument(
    collection: WithPath[Collection],
    parts: Seq[String],
    documentFacet: Site.DocumentFacet
  ): Option[Site.SiteFile] = if (parts.isEmpty || parts.tail.nonEmpty) None else {
    val (fileName: String, extension: Option[String]) = Files.nameAndExtension(parts.head)
    if (!extension.contains(documentFacet.extension)) None else {
      val document: Option[Document] = collection.value.by.get.stores.find { document =>
        document.by.get.stores.exists(teiHolder => teiHolder.name == fileName)
      }
      document.map(document => Site.SiteFile.DocumentFile(document, documentFacet))
    }
  }

  private def resolveEntity(parts: Seq[String]): Option[Site.SiteFile] = if (parts.isEmpty || parts.tail.nonEmpty) None else {
    val (fileName: String, extension: Option[String]) = Files.nameAndExtension(parts.head)
    store.entities.get.findByRef(fileName).flatMap { entity => extension match {
      case Some("html") => Some(Site.SiteFile.EntityFile(entity, Site.TeiFacet.Html))
      case Some("xml") => Some(Site.SiteFile.EntityFile(entity, Site.TeiFacet.Xml))
      case _ => None
    }}
  }

  private def resolveNamesFile(name: String, parts: Seq[String]): Option[Site.SiteFile] = if (parts.nonEmpty) None else {
    val (restOfTheName, extension) = Files.nameAndExtension(name)
    if (restOfTheName.nonEmpty) None else extension match {
      case Some("html") => Some(Site.SiteFile.NamesFile(Site.TeiFacet.Html))
      case Some("xml") => Some(Site.SiteFile.NamesFile(Site.TeiFacet.Xml))
      case _ => None
    }
  }
}

object Site {

  sealed abstract class TeiFacet(val extension: String)
  object TeiFacet {
    final case object Xml extends TeiFacet("xml")
    final case object Html extends TeiFacet("html")
  }

  sealed abstract class DocumentFacet(val extension: String)
  object DocumentFacet {
    final case object Tei extends DocumentFacet("xml")
    final case object Document extends DocumentFacet("html")
    final case object Facs extends DocumentFacet("html")
  }

  sealed trait SiteFile {
    def viewer: String
  }

  object SiteFile {
    final case class HierarchyFile(store: Store, teiFacet: TeiFacet) extends SiteFile {
      override def viewer: String = Site.collectionViewer
    }

    final case class CollectionIndex(collection: WithPath[Collection], teiFacet: TeiFacet) extends SiteFile {
      override def viewer: String = Site.collectionViewer
    }

    final case class DocumentFile(document: Document, teiFacet: DocumentFacet) extends SiteFile {
      override def viewer: String = Site.documentViewer
    }

    final case class EntityFile(entity: Entity, teiFacet: TeiFacet) extends SiteFile {
      override def viewer: String = Site.namesViewer
    }

    final case class NamesFile(teiFacet: TeiFacet) extends SiteFile {
      override def viewer: String = Site.namesViewer
    }
  }

  def removePart(from: String): String = {
    val sharp = from.indexOf('#')
    if (sharp == -1) from else from.substring(0, sharp)
  }
  private val hierarchyDirectoryName: String = "by"

  private val indexFileName: String = "index"

  private val collectionsFileName: String = "collections"

  // Note: also hard-coded in 'index.xml'!
  private val collectionsDirectoryName: String = "collections"

  private val namesDirectoryName: String = "names"

  private val namesFileName: String = "names"

  // Note: also hard-coded in _layouts/tei.html!
  private val facsDirectoryName: String = "facs" // facsimile viewers

  private val documentsDirectoryName: String = "documents" // wrappers for TEI XML

  private val teiDirectoryName: String = "tei"

  private def entityUrl(entity: Entity): String = s"/$namesDirectoryName/${entity.id.get}.html"

  private def entityInTheListUrl(id: String): String = s"/$namesFileName.html#$id"

  private def documentUrl(collection: Store, documentName: String): String =
    url(s"${fileName(collection)}/$documentsDirectoryName/$documentName.html")

  private def collectionUrl(collection: WithPath[Collection]): String =
    url(s"${collectionName(collection)}/index.html")

  private def url(ref: String): String = s"/$collectionsDirectoryName/$ref"

  private val namesHead: String = "Имена"

  private val unpublished: Set[String] = Set("derzhavin6", "derzhavin7", "lna208",
    "niab5", "niab19", "niab24", "rnb203", "rnb211")

  private val namesViewer: String = "namesViewer"

  private val collectionViewer: String = "collectionViewer"

  private val documentViewer: String = "documentViewer"

  private def fileName(store: Store): String =
    Files.nameAndExtension(Files.pathAndName(store.urls.fromUrl.get.getPath)._2)._1

  private def referenceCollectionName(reference: WithPath[EntityReference]): String =
    reference.path.init.init.last.store.names.name

  private def getName(names: Names): String = names.doFind(Language.Russian.toSpec).name

  // TODO this yuck is temporary :)

  private def collectionReference(collection: WithPath[Collection]): String =
    collection.value.names.name

  private def collectionTitle(collection: WithPath[Collection]): Seq[Node] =
    collection.value.title.fold[Seq[Node]](textNode(collectionReference(collection)))(_.xml)

  private def collectionDescription(collection: WithPath[Collection]): Seq[Node] =
    Seq(<span>{collection.value.storeAbstract.get.xml}</span>) ++
      RawXml.getXml(collection.value.body)

  private def collectionName(collection: WithPath[Collection]): String =
    fileName(collection.value)

  private def collectionArchive(collection: WithPath[Collection]): Option[String] = {
    val reference = collectionReference(collection)
    val space = reference.lastIndexOf(' ')
    if (space == -1) None else Some(reference.substring(0, space))
  }

  // TODO with images on a separate website (facsimiles.alter-rebbe.org), this has to be re-worked...
  //  private def checkPages(): Unit = {
  //    // Check that all the images are accounted for
  //    val imageNames: Set[String] =
  //      Util.filesWithExtensions(
  //        directory = layout.facsimiles(directory),
  //        ".jpg"
  //      )
  //      .toSet
  //    imageNames.foreach(name => pageType(name, isPresent = true))
  //
  //    val usedImages: Set[String] = pages.filter(_.isPresent).map(_.name).toSet
  //    val orphanImages: Seq[String] = (imageNames -- usedImages).toSeq.sorted
  //    val missingImages: Seq[String] = (usedImages -- imageNames).toSeq.sorted
  //    if (orphanImages.nonEmpty) throw new IllegalArgumentException(s"Orphan images: $orphanImages")
  //    if (missingImages.nonEmpty)
  //      throw new IllegalArgumentException(s"Missing images: $missingImages")
  //  }

  def write(
    directory: File,
    site: Site,
    references: Seq[WithPath[EntityReference]]
  ): Unit = {
    println("Writing site.")

    val namesDirectory: File = new File(directory, namesDirectoryName)
    Files.deleteFiles(namesDirectory)

    for (entity <- site.entities) writeTei(
      site,
      namesDirectory,
      fileName = entity.id.get,
      target = namesViewer,
      content = Seq(Entity.toXml(entity.copy(content = entity.content :+ mentions(entity, references))))
    )

    writeNamesList(
      site,
      directory
    )

    val collectionsDirectory: File = new File(directory, collectionsDirectoryName)
    Files.deleteFiles(collectionsDirectory)

    for (collection <- site.collections) {
      val collectionDirectory: File = new File(collectionsDirectory, collectionName(collection))
      writeCollectionIndex(site, collection, collectionDirectory)

      val teiDirectory: File = new File(collectionDirectory, teiDirectoryName)
      for ((document, (prev, next)) <- Collections.prevAndNext(collection.value.documents)) {
        for (teiHolder: TeiHolder <- document.by.get.stores) TeiUtil.teiPrettyPrinter.writeXml(
          file = new File(teiDirectory, teiHolder.name + ".xml"),
          elem = processTei(Tei.toXml(TeiUtil.addCommon(teiHolder.tei)), site)
        )

        // Wrappers
        val navigation: Seq[(String, String)] =
          Seq("documentCollection" -> quote(collectionReference(collection))) ++
          prev.map(prev => Seq("prevDocument" -> quote(prev.name))).getOrElse(Seq.empty) ++
          Seq("thisDocument" -> quote(document.name)) ++
          next.map(next => Seq("nextDocument" -> quote(next.name))).getOrElse(Seq.empty)

        writeTeiWrappers(new File(collectionDirectory, documentsDirectoryName), document, navigation)

        writeFacsViewer(
          facsDirectory = new File(collectionDirectory, facsDirectoryName),
          pageType = collection.value.pageType,
          document,
          navigation
        )
      }
    }

    writeIndex(site, directory)

    writeCollectionsTree(site, directory)

    writeHierarchy(site, directory)

    println("Writing reports.")

    writeReport(
      directory,
      name = "misnamed-entities",
      title = "Неправильно названные файлы с именами",
      content = site.entities.flatMap { entity =>
        val id = entity.id.get
        val expectedId = entity.name.replace(' ', '_')
        if (id == expectedId) None else Some(s"- '$id' должен по идее называться '$expectedId'")
      }
    )

    writeReport(
      directory,
      name = "no-refs",
      title = "Имена без атрибута 'ref'",
      content =
        for (reference <- references.filter(_.value.ref.isEmpty)) yield
          "- " + reference.value.name.map(_.text.trim).mkString(" ") + " в " +
            referenceCollectionName(reference) + ":" +
            reference.path.last.store.names.name
    )
  }

  private def writeHierarchy(site: Site, directory: File): Unit = {
    val hierarchyDirectory: File = new File(directory, hierarchyDirectoryName)
    Files.deleteFiles(hierarchyDirectory)
    for (store <- site.stores) writeTei(
      site,
      directory = file(hierarchyDirectory, store.path),
      fileName = "index",
      target = collectionViewer,
      content = store2xml(store)
    )
  }

  private def store2xml(store: WithPath[Store]): Seq[Node] = {
    storeHeader(store) ++
    store.value.by.toSeq.flatMap { by: By[_] =>
      <p>
      <l>{getName(by.selector.names)}:</l>
      <list type="bulleted">
      {by.stores.map { storeX =>
        val subStore = storeX.asInstanceOf[Store]  // TODO get rid of the cast!!!
        val title: Seq[Node] = RawXml.getXml(subStore.title)
        val titlePrefix: Seq[Node] = textNode(getName(subStore.names) + (if (title.isEmpty) "" else ": "))
        <item>
          {ref(
          url =
            if (subStore.isInstanceOf[Collection]) s"/$collectionsDirectoryName/${fileName(subStore)}"
            else path2url(store.path :+ by.selector.bind(subStore)),
          text = titlePrefix ++ title
        )}</item>
      }}
      </list>
      </p>
    }
  }

  private def storeHeader(store: WithPath[Store]): Seq[Node] = {
//    println(segments(store.path).mkString("/"))
    val isTop: Boolean = store.path.isEmpty
    val title: Seq[Node] = RawXml.getXml(store.value.title)
    val titlePrefix: Seq[Node] =
      if (isTop) Seq.empty else textNode(
        getName(store.path.last.selector.names) + " " + getName(store.value.names) + (if (title.isEmpty) "" else ": ")
      )

    pathLinks(if (isTop) store.path else store.path.init) ++
    <head>{titlePrefix ++ title}</head> ++
    store.value.storeAbstract.map(value => <span>{value.xml}</span>).getOrElse(Seq.empty) ++
    RawXml.getXml(store.value.body)
  }

  private def pathLinks(path: Path): Seq[Elem] = for (ancestor <- path.path.inits.toSeq.reverse.tail) yield {
    val binding: Binding = ancestor.last
    val link: Elem = ref(
      url = path2url(Path(ancestor)),
      text = getName(binding.store.names)
    )
    val title: Seq[Node] = RawXml.getXml(binding.store.title)
    val titlePrefix: Seq[Node] = if (title.isEmpty) Seq.empty else Seq(textNode(": "))
    <l>{getName(binding.selector.names)} {link ++ titlePrefix ++ title}</l>
  }

  private def file(directory: File, path: Path): File =
    file(directory, segments(path))

  @scala.annotation.tailrec
  private def file(directory: File, segments: Seq[String]): File =
    if (segments.isEmpty) directory
    else file(new File(directory, segments.head), segments.tail)

  private def path2url(path: Path): String =
      "/" + hierarchyDirectoryName + "/" + segments(path).mkString("", "/", "/")

  private def segments(path: Path): Seq[String] =
    path.path.flatMap(binding => Seq(binding.selector.names, binding.store.names))
      .map(getName)
      .map(_.replace(' ', '_'))

  private def writeCollectionIndex(
    site: Site,
    collection: WithPath[Collection],
    directory: File
  ): Unit = {
    val missingPages: Seq[String] = collection.value.documents
      .flatMap(document => document.pages(collection.value.pageType))
      .filterNot(_.isPresent)
      .map(_.displayName)

    writeTei(
      site,
      directory,
      fileName = "index",
      content =
// TODO insert path information into the case descriptors:
// and remove next line storeHeader(collection) ++
        <head>{collectionTitle(collection)}</head> ++ collectionDescription(collection) ++
        Seq[Elem](table(collection.value).toTei(
          collection.value.parts.flatMap { part =>
            part.title.fold[Seq[Node]](Seq.empty)(_.xml).map(Table.Xml) ++
              part.documents.map(Table.Data[Document]) }
        )) ++
        (if (missingPages.isEmpty) Seq.empty
        else Seq(<p>Отсутствуют фотографии {missingPages.length} страниц: {missingPages.mkString(" ")}</p>)),
      style = Some("wide"),
      target = collectionViewer,
      yaml = Seq("documentCollection" -> quote(collectionReference(collection)))
    )
  }

  private def table(collection: Collection): Table[Document] = new Table[Document](
    Table.Column("Описание", "description", { document: Document =>
        document.tei.getAbstract
// Ignoring the titles:          .orElse(document.tei.titleStmt.titles.headOption.map(_.xml))
          .getOrElse(Seq.empty)
          .map(XmlUtil.removeNamespace)
    }),

    Table.Column("Дата", "date", { document: Document =>
      textNode(document.tei.creationDate.map(_.when).getOrElse(""))
    }),

    Table.Column("Кто", "author", { document: Document =>
      val authors = document.tei.titleStmt.authors.map(_.xml).flatMap(_.map(XmlUtil.removeNamespace))
      multi(authors)
    }),

    Table.Column("Кому", "addressee",  { document =>
      document.tei.addressee.fold[Seq[Node]](textNode(""))(addressee =>
        <persName ref={addressee.ref.orNull}>{addressee.name}</persName>)
    }),

    Table.Column("Язык", "language", { document: Document =>
      val translations: Seq[Elem] =
        for (teiHolder <- document.by.get.stores.filter(_.language.isDefined)) yield translationRef(collection, teiHolder)
      val language: Option[String] = document.tei.languages.map(_.ident).headOption
        .orElse(document.tei.text.lang)
      Seq(textNode(language.getOrElse("?"))) ++ translations
    }),

    Table.Column("Документ", "document", { document: Document => documentRef(collection, document) }),

    Table.Column("Страницы", "pages", { document: Document =>
      for (page <- document.pages(collection.pageType)) yield documentPageRef(collection, document, page) }),

    Table.Column("Расшифровка", "transcriber", { document: Document =>
      val transcribers = document.tei.titleStmt.editors
        .filter(_.role.contains("transcriber")).flatMap(_.persName)
        .map(transcriber => XmlUtil.removeNamespace(EntityReference.toXml(transcriber)))
      multi(transcribers)
    })
  )

  private def writeTeiWrappers(
    docsDirectory: File,
    document: Document,
    navigation: Seq[(String, String)]
  ): Unit = {

    def forTeiHolder(teiHolder: TeiHolder): Unit = {
      val name: String = teiHolder.name
      writeTeiWrapper(
        directory = docsDirectory,
        fileName = name,
        teiPrefix = Some(s"../$teiDirectoryName/"), // TODO get rid of the relative URLs
        target = documentViewer,
        yaml = Seq(
          "facs" -> s"'../$facsDirectoryName/${document.name}.html'"  // TODO get rid of the relative URLs
        ) ++ (
          if (teiHolder.language.isDefined || document.languages.isEmpty) Seq.empty
          else Seq("translations" -> document.languages.mkString("[", ", ", "]"))) ++ navigation
      )
    }

    for (teiHolder <- document.by.get.stores) forTeiHolder(teiHolder)
  }

  private def writeFacsViewer(
    facsDirectory: File,
    pageType: Page.Type,
    document: Document,
    navigation: Seq[(String, String)]
  ): Unit = {
    // TODO get rid of the relative URLs
    val facsimilePages: Elem =
      <div class="facsimileViewer">
        <div class="facsimileScroller">{
          for (page: Page <- document.pages(pageType).filter(_.isPresent); n = page.n) yield {
            <a target={documentViewer} href={s"../$documentsDirectoryName/${document.name}.html#p$n"}>
              <figure>
                <img xml:id={s"p$n"} alt={s"facsimile for page $n"} src={page.facs.orNull}/>
                <figcaption>{n}</figcaption>
              </figure>
            </a>}}
        </div>
      </div>

    // TODO get rid of the relative URLs
    writeWithYaml(
      file = new File(facsDirectory, document.name + ".html"),
      layout = "default",
      yaml = Seq(
        "transcript" -> s"'../$documentsDirectoryName/${document.name}.html'"
      )
        ++ navigation,
      content = Seq(TeiUtil.htmlPrettyPrinter.render(facsimilePages) + "\n")
    )
  }

  // TODO write new, truly hierarchical index!
  private def writeCollectionsTree(site: Site, directory: File): Unit = {
    val byArchive: Map[String, Seq[WithPath[Collection]]] =
      site.collections.groupBy(collection => collectionArchive(collection).getOrElse(""))
    writeTei(
      site,
      directory,
      fileName = collectionsFileName,
      content =
        <head>Архивы</head> ++
        <list>{
        for (archive <- byArchive.keys.toList.sorted) yield {
          <item>
            <p>{s"[$archive]"}</p>
            <list type="bulleted">{for (collection <- byArchive(archive)) yield toXml(collection)}</list>
          </item>}}
      </list>,
      target = collectionViewer,
      yaml = Seq("title" -> "Архивы")
    )
  }

  // TODO add nomenclature from the path to the info line:
  private def writeIndex(site: Site, directory: File): Unit = writeTei(
    site,
    directory,
    fileName = indexFileName,
    content =
      <head>Дела</head> ++
      <list type="bulleted">
      {for (collection <- site.collections.filterNot(collection => unpublished.contains(collectionName(collection))))
       yield toXml(collection)}
      </list>,
    target = collectionViewer,
    yaml = Seq("windowName" -> collectionViewer)
  )

  private def toXml(collection: WithPath[Collection]): Elem = {
    val url = collectionUrl(collection)
    // If I do this, parts of the line click to the names... {ref(url, collectionViewer, textNode(collectionReference(collection) + ": ") ++ collectionTitle(collection))}<lb/>
    <item>
      {ref(url, collectionReference(collection) + ": " +
        spacedText(collectionTitle(collection)))}<lb/>
      <abstract>{collection.value.storeAbstract.get.xml}</abstract>
    </item>
  }

  // TODO clean up!
  private def mentions(
    value: Entity,
    references: Seq[WithPath[EntityReference]]
  ): Elem = {

    def sources(references: Seq[WithPath[EntityReference]]): Seq[Elem] = {
      // TODO grouping needs to be adjusted to handle references from collection descriptors;
      // once fund, опись etc. have their own URLs, they should be included too.
      val result: Seq[Option[Elem]] =
      for (source <- Collections.removeConsecutiveDuplicates(references.map(_.path))) yield {
        val sourceStore: Store = source.last.store
        val url: Option[String] = sourceStore match {
          case teiHolder: TeiHolder => Some(documentUrl(source.init.init.last.store, fileName(teiHolder)))
          case document: Document => Some(documentUrl(source.init.last.store, fileName(document)))
          case collection: Collection => None // TODO Some(collectionUrl(collection)) when grouping is adjusted
          case _ => None
        }
        url.map(url => ref(url, sourceStore.names.name))
      }

      result.flatten
    }

    val (fromEntities: Seq[WithPath[EntityReference]], notFromNames: Seq[WithPath[EntityReference]]) = references
      .filter(_.value.ref.contains(value.id.get))
      .partition(_.path.last.store.isInstanceOf[EntityHolder])

    val bySource: Seq[(String, Seq[WithPath[EntityReference]])] =
      notFromNames
        .filter(reference => (reference.path.length >=3) && reference.path.init.init.last.store.isInstanceOf[Collection])  // TODO remove when grouping is adjusted
        .groupBy(referenceCollectionName).toSeq.sortBy(_._1)

    <p rendition="mentions">
      {ref(entityInTheListUrl(value.id.get), "[...]")}
      {if (fromEntities.isEmpty) Seq.empty else {
      <l>
        <emph>{namesHead}:</emph>
        {
        val result =
          for (source <- Collections.removeConsecutiveDuplicates(fromEntities.map(_.path)))
          yield entityRef(source.last.store.asInstanceOf[EntityHolder])
        result.init.map(elem => <span>{elem},</span>) :+ result.last
        }
      </l>}}
      {for ((source, references) <- bySource)
       yield <l><emph>{source}:</emph>{sources(references)}</l>}
    </p>
  }

  private def writeNamesList(
    site: Site,
    directory: File
  ): Unit = {
    val nonEmptyLists: Seq[EntitiesList] = site.entitiesLists.filterNot(_.isEmpty)
    val listOfLists: Seq[Node] =
      <p>{for (list <- nonEmptyLists) yield <l>{ref(entityInTheListUrl(list.id), list.head)}</l>}</p>

    def toXml(value: EntitiesList): Elem =
      <list xml:id={value.id} role={value.role.orNull}>
        <head>{value.head}</head>
        {for (entity <- value.entities) yield {
        <l>{ref(entityUrl(entity), EntityName.toXml(entity.names.head))}</l>
      }}
      </list>
        .copy(label = value.entityType.listElement)

    writeTei(
      site,
      directory,
      fileName = namesFileName,
      content = <head>{namesHead}</head> ++ listOfLists ++ nonEmptyLists.flatMap(toXml),
      target = namesViewer,
      yaml = Seq("title" -> namesHead)
    )
  }

  private def entityRef(entityHolder: EntityHolder): Elem = ref(
    url = entityUrl(entityHolder.entity),
    text = entityHolder.names.name
  )

  private def documentRef(collection: Collection, document: Document): Elem =
    ref(documentUrl(collection, document.name), document.name)

  private def translationRef(collection: Collection, teiHolder: TeiHolder): Elem =
    ref(documentUrl(collection, teiHolder.name), teiHolder.language.get)

  private def documentPageRef(collection: Collection, document: Document, page: Page): Elem =
    ref(documentUrl(collection, document.name) + s"#p${page.n}", page.displayName,
      Some(if (page.isPresent) "page" else "missing-page"))

  private def ref(
    url: String,
    text: String,
    css: Option[String] = None
  ): Elem = <ref target={url} rendition={css.orNull}>{text}</ref>

  private def ref(
    url: String,
    text: Seq[Node]
  ): Elem = <ref target={url}>{text}</ref>

  private def writeTei(
    site: Site,
    directory: File,
    fileName: String,
    content: Seq[Node],
    style: Option[String] = None,
    target: String,
    yaml: Seq[(String, String)] = Seq.empty
  ): Unit = {
    TeiUtil.teiPrettyPrinter.writeXml(
      file = new File(directory, fileName + ".xml"),
      elem = processTei(Tei.toXml(TeiUtil.addCommonNoCalendar(Tei(content))), site)
    )

    writeTeiWrapper(
      directory,
      fileName,
      teiPrefix = None,
      style,
      target,
      yaml
    )
  }

  private def refRoleRewriter(site: Site): Elem => Elem = elem =>
    if (elem.label != "ref") elem else {
      elem.attribute("target").map(_.text).fold(throw new IllegalArgumentException("empty target!")) { target =>
        if (!target.startsWith("/")) elem else {
          val roleShouldBe: Option[String] = site.resolve(target).map(_.viewer)
          val role: Option[String] = elem.attribute("role").map(_.text)
          if (roleShouldBe.isEmpty) println(s"did not resolve: $target")
          if (roleShouldBe.isDefined && role.isDefined && (role != roleShouldBe)) println(s"role discrepancy")
          if ((role == roleShouldBe) || roleShouldBe.isEmpty || role.isDefined) elem
          else elem % scala.xml.Attribute(None, "role", textNode(roleShouldBe.get), scala.xml.Null)
        }
      }
    }

  private def processTei(elem: Elem, site: Site): Elem =
    XmlUtil.rewriteElements(elem, refRoleRewriter(site))

  // TODO calculate viewer from the URL when writing Site.SiteFile:
  private def writeTeiWrapper(
    directory: File,
    fileName: String,
    teiPrefix: Option[String] = None,
    style: Option[String] = None,
    target: String,
    yaml: Seq[(String, String)]
  ): Unit = writeWithYaml(
    file = new File(directory, fileName + ".html"),
    layout = "tei",
    yaml =
      style.fold[Seq[(String, String)]](Seq.empty)(style => Seq("style" -> style)) ++
        Seq(
          "tei" -> quote(teiPrefix.getOrElse("") + fileName + ".xml"),
          "target" -> target
        ) ++ yaml
  )

  private def writeWithYaml(
    file: File,
    layout: String,
    yaml: Seq[(String, String)],
    content: Seq[String] = Seq.empty
  ): Unit = {
    val result: Seq[String] =
      Seq("---") ++
        (for ((name, value) <- ("layout", layout) +: yaml) yield name + ": " + value) ++
        Seq("---") ++
        Seq("") ++ content

    Files.write(file, result.mkString("\n"))
  }

  private def writeReport(
    directory: File,
    name: String,
    title: String,
    content: Seq[String]
  ): Unit = writeWithYaml(
    file = new File(new File(directory, "reports"), name + ".md"),
    layout = "page",
    yaml = Seq("title" -> title),
    content :+ "\n"
  )

  private def multi(nodes: Seq[Node]): Seq[Node] = nodes match {
    case Nil => Nil
    case n :: Nil => Seq(n)
    case n :: ns if n.isInstanceOf[Elem] => Seq(n, textNode(", ")) ++ multi(ns)
    case n :: ns => Seq(n) ++ multi(ns)
    case n => n
  }

  def textNode(text: String): Node = new scala.xml.Text(text)

  def spacedText(nodes: Seq[Node]): String = nodes.map(spacedText).mkString("")
  def spacedText(node: Node): String = {
    val result = node match {
      case elem: Elem => (elem.child map (_.text)).mkString(" ")
      case node: Node => node.text
    }
    result
      .replace('\n', ' ')
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
  }

  private def quote(what: String): String = s"'$what'"
}
