package org.opentorah.archive.collector

import java.io.File
import org.opentorah.entity.{EntitiesList, Entity, EntityName, EntityReference}
import org.opentorah.store.{EntityHolder, Store, TeiHolder, WithPath}
import org.opentorah.tei.Tei
import org.opentorah.util.{Collections, Files}
import org.opentorah.xml.XmlUtil
import scala.xml.{Elem, Node}

object Site {

  private val indexFileName: String = "index"

  private val collectionsFileName: String = "collections"

  // TODO Note: also hard-coded in 'index.xml'!
  private val collectionsDirectoryName: String = "collections"

  private val namesDirectoryName: String = "names"

  private val namesFileName: String = "names"

  // TODO Note: also hard-coded in _layouts/tei.html!
  private val facsDirectoryName: String = "facs" // facsimile viewers

  private val documentsDirectoryName: String = "documents"

  private val teiDirectoryName: String = "tei"

  private def entityUrl(entity: Entity): String = s"/$namesDirectoryName/${entity.id.get}.html"

  private def entityInTheListUrl(id: String): String = s"/$namesFileName.html#$id"

  private def documentUrl(collection: Store, documentName: String): String =
    url(s"${fileName(collection)}/${documentUrlRelativeToIndex(documentName)}")

  private def documentUrlRelativeToIndex(name: String): String =  s"$docsDirectoryName/$name.html"

  private val docsDirectoryName: String = "documents" // wrappers for TEI XML

  private def collectionUrl(collection: Collection): String =
    url(s"${collectionName(collection)}/index.html")

  private def url(ref: String): String = s"/$collectionsDirectoryName/$ref"

  private val namesHead: String = "Имена"

  private val books: Set[String] = Set("Державин", "Дубнов")

  private val unpublished: Set[String] = Set("derzhavin", "lna208", "niab5", "niab19", "niab24", "rnb203", "rnb211")

  private val namesViewer: String = "namesViewer"

  private def fileName(store: Store): String =
    Files.nameAndExtension(Files.pathAndName(store.fromUrl.get.getPath)._2)._1

  private def referenceCollectionName(reference: WithPath[EntityReference]): String =
    reference.path.init.init.last.store.names.name

  // TODO this yuck is temporary :)

  private def collectionReference(collection: Collection): String =
    collection.names.name

  private def collectionTitle(collection: Collection): Node =
    collection.title.fold[Node](scala.xml.Text(collectionReference(collection)))(title => <title>{title.xml}</title>)

  private def collectionDescription(collection: Collection): Seq[Node] =
    Seq(<span>{collection.storeAbstract.get.xml}</span>) ++ collection.notes.xml

  private def collectionPageType(collection: Collection): Page.Type =
    if (books.contains(collectionReference(collection))) Page.Book else Page.Manuscript

  private def collectionName(collection: Collection): String =
    fileName(collection)

  private def collectionArchive(collection: Collection): Option[String] = {
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
    store: Store,
    references: Seq[WithPath[EntityReference]]
  ): Unit = {
    val lists: Seq[EntitiesList] = store.entities.get.lists
    val entities: Seq[Entity] = store.entities.get.by.get.stores.map(_.entity)

    println("Writing site.")

    val namesDirectory: File = new File(directory, namesDirectoryName)
    Files.deleteFiles(namesDirectory)

    for (entity <- entities) writeTei(
      namesDirectory,
      fileName = entity.id.get,
      head = None,
      target = namesViewer,
      content = Seq(Entity.toXml(entity.copy(content = entity.content :+ mentions(entity, references))))
    )

    writeNamesList(
      nonEmptyLists = lists.filterNot(_.isEmpty),
      directory
    )

    val collectionsDirectory: File = new File(directory, collectionsDirectoryName)
    Files.deleteFiles(collectionsDirectory)

    val collections: Seq[WithPath[Collection]] = store.withPath[Collection](values = {
      case collection: Collection => Seq(collection)
      case _ => Seq.empty
    })

    for (collection <- collections) {
      // TODO insert path information into the case descriptors.
      val collectionDirectory: File = new File(collectionsDirectory, collectionName(collection.value))
      writeCollectionIndex(collection.value, collectionDirectory)

      val teiDirectory: File = new File(collectionDirectory, teiDirectoryName)
      for ((document, (prev, next)) <- Collections.prevAndNext(collection.value.documents)) {
        for (teiHolder: TeiHolder <- document.by.get.stores) TeiUtil.teiPrettyPrinter.writeXml(
          file = new File(teiDirectory, teiHolder.name + ".xml"),
          elem = Tei.toXml(TeiUtil.addCommon(teiHolder.tei))
        )

        // Wrappers
        val navigation: Seq[(String, String)] =
          Seq("documentCollection" -> quote(collectionReference(collection.value))) ++
          prev.map(prev => Seq("prevDocument" -> quote(prev.name))).getOrElse(Seq.empty) ++
          Seq("thisDocument" -> quote(document.name)) ++
          next.map(next => Seq("nextDocument" -> quote(next.name))).getOrElse(Seq.empty)

        writeTeiWrappers(new File(collectionDirectory, docsDirectoryName), document, navigation)

        writeFacsViewer(
          facsDirectory = new File(collectionDirectory, facsDirectoryName),
          pageType = collectionPageType(collection.value),
          document,
          navigation
        )
      }
    }

    writeIndex(collections.map(_.value), directory)

    // TODO write new, truly hierarchical index!
    writeCollectionsTree(collections.map(_.value), directory)

    println("Writing reports.")

    writeReport(
      directory,
      name = "misnamed-entities",
      title = "Неправильно названные файлы с именами",
      content = entities.flatMap { entity =>
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

  private def writeCollectionIndex(
    collection: Collection,
    directory: File
  ): Unit = {
    val missingPages: Seq[String] = collection.documents
      .flatMap(document => document.pages(collectionPageType(collection)))
      .filterNot(_.isPresent)
      .map(_.displayName)

    writeTei(
      directory,
      fileName = "index",
      head = Some(collectionTitle(collection)),
      content = collectionDescription(collection) ++
        Seq[Elem](table(collectionPageType(collection), documentUrlRelativeToIndex).toTei(
          collection.parts.flatMap { part =>
            part.title.fold[Seq[Node]](Seq.empty)(_.xml).map(Table.Xml) ++
              part.documents.map(Table.Data[Document]) }
        )) ++
        (if (missingPages.isEmpty) Seq.empty
        else Seq(<p>Отсутствуют фотографии {missingPages.length} страниц: {missingPages.mkString(" ")}</p>)),
      style = Some("wide"),
      target = "collectionViewer",
      yaml = Seq("documentCollection" -> quote(collectionReference(collection)))
    )
  }

  private def table(
    pageType: Page.Type,
    documentUrlRelativeToIndex: String => String
  ): Table[Document] = new Table[Document](
    Table.Column("Описание", "description", { document: Document =>
        document.tei.getAbstract
          .orElse(document.tei.titleStmt.titles.headOption.map(_.content))
          .getOrElse(Seq.empty)
          .map(XmlUtil.removeNamespace)
    }),

    Table.Column("Дата", "date", { document: Document =>
      scala.xml.Text(document.tei.creationDate.map(_.when).getOrElse(""))
    }),

    Table.Column("Кто", "author", { document: Document =>
      val authors = document.tei.titleStmt.authors.map(_.xml).flatMap(_.map(XmlUtil.removeNamespace))
      multi(authors)
    }),

    Table.Column("Кому", "addressee",  { document =>
      document.tei.addressee.fold[Seq[Node]](scala.xml.Text(""))(addressee =>
        <persName ref={addressee.ref.orNull}>{addressee.name}</persName>)
    }),

    Table.Column("Язык", "language", { document: Document =>
      val translations: Seq[Elem] = for (language <- document.languages) yield translationRef(document, language)
      val language: Option[String] = document.tei.languages.map(_.ident).headOption
      Seq(scala.xml.Text(language.getOrElse("?"))) ++ translations
    }),

    Table.Column("Документ", "document", { document: Document => documentRef(document) }),

    Table.Column("Страницы", "pages", { document: Document =>
      for (page <- document.pages(pageType)) yield documentPageRef(document, page) }),

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

    def writeTeiWrapperForLang(name: String, lang: Option[String]): Unit = writeTeiWrapper(
      directory = docsDirectory,
      fileName = lang.fold(name)(lang => name + "-" + lang),
      teiPrefix = Some(s"../$teiDirectoryName/"),
      target = "documentViewer",
      yaml = Seq(
        "facs" -> s"'../$facsDirectoryName/$name.html'"
      ) ++ (
        if (lang.isDefined || document.languages.isEmpty) Seq.empty
        else Seq("translations" -> document.languages.mkString("[", ", ", "]"))) ++ navigation
      )

    // TEI wrapper(s)
    writeTeiWrapperForLang(document.name, None)
    for (lang <- document.languages) writeTeiWrapperForLang(document.name, Some(lang))
  }

  private def writeFacsViewer(
    facsDirectory: File,
    pageType: Page.Type,
    document: Document,
    navigation: Seq[(String, String)]
  ): Unit = {
    val facsimilePages: Elem =
      <div class="facsimileViewer">
        <div class="facsimileScroller">{
          for (page: Page <- document.pages(pageType).filter(_.isPresent); n = page.n) yield {
            <a target="documentViewer" href={s"../$documentsDirectoryName/${document.name}.html#p$n"}>
              <figure>
                <img xml:id={s"p$n"} alt={s"facsimile for page $n"} src={page.facs.orNull}/>
                <figcaption>{n}</figcaption>
              </figure>
            </a>}}
        </div>
      </div>

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

  private def writeCollectionsTree(collections: Seq[Collection], directory: File): Unit = {
    val byArchive: Map[String, Seq[Collection]] = collections.groupBy(collection => collectionArchive(collection).getOrElse(""))
    writeTei(
      directory,
      fileName = collectionsFileName,
      head = Some(scala.xml.Text("Архивы")),
      content = <list>{
        for (archive <- byArchive.keys.toList.sorted) yield {
          <item>
            <p>{s"[$archive]"}</p>
            <list type="bulleted">{for (collection <- byArchive(archive)) yield toXml(collection)}</list>
          </item>}}
      </list>,
      target = "collectionViewer"
    )
  }

  private def writeIndex(collections: Seq[Collection], directory: File): Unit = writeTei(
    directory,
    fileName = indexFileName,
    head = Some(scala.xml.Text("Дела")),
    content =
      <list type="bulleted">
      {for (collection <- collections.filterNot(collection => unpublished.contains(collectionName(collection))))
       yield toXml(collection)}
      </list>,
    target = "collectionViewer",
    yaml = Seq("windowName" -> "collectionViewer")
  )

  // TODO moveXmlUtil.spacedText() here :)
  private def toXml(collection: Collection): Elem = {
    val url = collectionUrl(collection)
    <item>
      {ref(url, "collectionViewer", collectionReference(collection) + ": " +
        XmlUtil.spacedText(collectionTitle(collection)))}<lb/>
      <abstract>{collection.storeAbstract.get.xml}</abstract>
    </item>
  }

  // TODO clean up!
  private def mentions(
    value: Entity,
    references: Seq[WithPath[EntityReference]]
  ): Elem = {

    def sources(viewer: String, references: Seq[WithPath[EntityReference]]): Seq[Elem] = {
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
        url.map(url => ref(url, viewer, sourceStore.names.name))
      }

      result.flatten
    }

    val (fromEntities: Seq[WithPath[EntityReference]], notFromNames: Seq[WithPath[EntityReference]]) = references
      .filter(_.value.ref.contains(value.id.get))
      .partition(_.path.last.store.isInstanceOf[EntityHolder])

    val bySource: Seq[(String, Seq[WithPath[EntityReference]])] =
      notFromNames
        .filter(reference => (reference.path.path.length >=3) && reference.path.init.init.last.store.isInstanceOf[Collection])  // TODO remove when grouping is adjusted
        .groupBy(referenceCollectionName).toSeq.sortBy(_._1)

    <p rendition="mentions">
      {ref(entityInTheListUrl(value.id.get), namesViewer, "[...]")}
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
       yield <l><emph>{source}:</emph>{sources("documentViewer", references)}</l>}
    </p>
  }

  private def writeNamesList(nonEmptyLists: Seq[EntitiesList], directory: File): Unit = {
    val listOfLists: Seq[Node] =
      <p>{for (list <- nonEmptyLists) yield <l>{ref(entityInTheListUrl(list.id), namesViewer, list.head)}</l>}</p>

    def toXml(value: EntitiesList): Elem =
      <list xml:id={value.id} role={value.role.orNull}>
        <head>{value.head}</head>
        {for (entity <- value.entities) yield {
        <l>{ref(entityUrl(entity), namesViewer, EntityName.toXml(entity.names.head))}</l>
      }}
      </list>
        .copy(label = value.entityType.listElement)

    writeTei(
      directory,
      fileName = namesFileName,
      head = Some(scala.xml.Text(namesHead)),
      content = listOfLists ++ nonEmptyLists.flatMap(toXml),
      target = namesViewer
    )
  }

  private def entityRef(entityHolder: EntityHolder): Elem = ref(
    url = entityUrl(entityHolder.entity),
    viewer = namesViewer,
    text = entityHolder.names.name
  )

  private def documentRef(document: Document): Elem =
    ref(documentUrlRelativeToIndex(document.name), "documentViewer", document.name)

  private def translationRef(document: Document, language: String): Elem =
    ref(documentUrlRelativeToIndex(document.name + "-" + language), "documentViewer", language)

  private def documentPageRef(document: Document, page: Page): Elem =
    ref(documentUrlRelativeToIndex(document.name) + s"#p${page.n}", "documentViewer", page.displayName,
      Some(if (page.isPresent) "page" else "missing-page"))

  private def ref(
    url: String,
    viewer: String,
    text: String,
    css: Option[String] = None
  ): Elem = <ref target={url} role={viewer} rendition={css.orNull}>{text}</ref>

  private def ref(
    url: String,
    viewer: String,
    text: Elem
  ): Elem = <ref target={url} role={viewer}>{text}</ref>

  private def writeTei(
    directory: File,
    fileName: String,
    head: Option[Node],
    content: Seq[Node],
    style: Option[String] = None,
    target: String,
    yaml: Seq[(String, String)] = Seq.empty
  ): Unit = {
    val body: Seq[Node] =
      head.fold[Seq[Node]](Seq.empty)(head => Seq(<head>{head}</head>)) ++ content

    TeiUtil.teiPrettyPrinter.writeXml(
      file = new File(directory, fileName + ".xml"),
      elem = Tei.toXml(TeiUtil.addCommonNoCalendar(Tei(body)))
    )

    val titleYaml: Seq[(String, String)] =
      head.fold[Seq[(String, String)]](Seq.empty)(head => Seq("title" -> quote(XmlUtil.spacedText(head))))

    writeTeiWrapper(
      directory,
      fileName,
      teiPrefix = None,
      style,
      target,
      yaml = titleYaml ++ yaml
    )
  }

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
    case n :: ns if n.isInstanceOf[Elem] => Seq(n, scala.xml.Text(", ")) ++ multi(ns)
    case n :: ns => Seq(n) ++ multi(ns)
    case n => n
  }

  private def quote(what: String): String = s"'$what'"
}
