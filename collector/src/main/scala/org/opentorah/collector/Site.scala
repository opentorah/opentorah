package org.opentorah.collector

import java.io.File
import org.opentorah.entity.{EntitiesList, Entity, EntityReference}
import org.opentorah.metadata.{Language, Names}
import org.opentorah.store.{Entities, EntityHolder, Path, Store, WithPath}
import org.opentorah.tei.Tei
import org.opentorah.util.{Collections, Files}
import org.opentorah.xml.{RawXml, XmlUtil}
import scala.xml.{Elem, Node}

final class Site(store: Store, val references: Seq[WithPath[EntityReference]]) {

  val stores: Seq[WithPath[Store]] = store.withPath[Store](values = {
    case _: Collection | _: Document | _: Entities | _: EntityHolder | _: TeiHolder => Seq.empty
    case store => Seq(store)
  })

  val collections: Seq[WithPath[Collection]] = store.withPath[Collection](values = {
    case collection: Collection => Seq(collection)
    case _ => Seq.empty
  })

  val entities: Seq[Entity] = store.entities.get.by.get.stores.map(_.entity)

  def findByRef(fileName: String): Option[Entity] =  store.entities.get.findByRef(fileName)

  val entitiesLists: Seq[EntitiesList] = store.entities.get.lists.filterNot(_.isEmpty)

  def namesObject: NamesObject = new NamesObject(this)

  def resolve(url: String): Option[SiteFile] = {
    if (!url.startsWith("/")) None else {
      val parts: Seq[String] = Site.removePart(url).substring(1).split("/")
      if (parts.isEmpty) None /* TODO site index... */ else {
        val tail: Seq[String] = parts.tail
        parts.head match {
          case HierarchyObject.hierarchyDirectoryName =>
            HierarchyObject.resolve(this, Path.empty, store, tail)

          case Site.collectionsDirectoryName =>
            CollectionObject.resolve(this, tail)

          case EntityObject.namesDirectoryName =>
            EntityObject.resolve(this, tail)

          case file =>
            val (fileName: String, extension: Option[String]) = Files.nameAndExtension(file)
            if (fileName == NamesObject.namesFileName) NamesObject.resolve(this, extension, tail)
            else None
        }
      }
    }
  }
}

object Site {

  def removePart(from: String): String = {
    val sharp = from.indexOf('#')
    if (sharp == -1) from else from.substring(0, sharp)
  }

  private val indexFileName: String = "index"

  private val collectionsFileName: String = "collections"

  // Note: also hard-coded in 'index.xml'!
  val collectionsDirectoryName: String = "collections"

  // Note: also hard-coded in _layouts/tei.html!
  val facsDirectoryName: String = "facs" // facsimile viewers

  val documentsDirectoryName: String = "documents" // wrappers for TEI XML

  val teiDirectoryName: String = "tei"

  def documentUrl(collection: Store, documentName: String): String =
    url(s"${fileName(collection)}/$documentsDirectoryName/$documentName.html")

  private def collectionUrl(collection: WithPath[Collection]): String =
    url(s"${collectionName(collection)}/index.html")

  private def url(ref: String): String = s"/$collectionsDirectoryName/$ref"

  private val unpublished: Set[String] = Set("derzhavin6", "derzhavin7", "lna208",
    "niab5", "niab19", "niab24", "rnb203", "rnb211")

  val collectionViewer: String = "collectionViewer"

  val documentViewer: String = "documentViewer"

  def fileName(store: Store): String =
    Files.nameAndExtension(Files.pathAndName(store.urls.fromUrl.get.getPath)._2)._1

  def referenceCollectionName(reference: WithPath[EntityReference]): String =
    reference.path.init.init.last.store.names.name

  def getName(names: Names): String = names.doFind(Language.Russian.toSpec).name

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

  // TODO make a method on Site
  def write(
    directory: File,
    site: Site
  ): Unit = {
    println("Writing site.")

    writeSiteObject(site.namesObject, directory)

    val namesDirectory: File = new File(directory, EntityObject.namesDirectoryName)
    Files.deleteFiles(namesDirectory)
    for (entity <- site.entities) {
      writeSiteObject(new EntityObject(site, entity), directory)
    }

    val hierarchyDirectory: File = new File(directory, HierarchyObject.hierarchyDirectoryName)
    Files.deleteFiles(hierarchyDirectory)
    for (store <- site.stores) writeSiteObject(new HierarchyObject(site, store.path, store.value), directory)

    val collectionsDirectory: File = new File(directory, collectionsDirectoryName)
    Files.deleteFiles(collectionsDirectory)

    for (collection <- site.collections) {
      val collectionDirectory: File = new File(collectionsDirectory, collectionName(collection))
      writeCollectionIndex(site, collection, collectionDirectory)
    }

    for (collection <- site.collections) {
      val collectionDirectory: File = new File(collectionsDirectory, collectionName(collection))
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

    println("Writing reports.")

    writeReport(
      directory,
      name = "misnamed-entities",
      title = "Неправильно названные файлы с именами",
      content = site.entities.flatMap { entity =>
        val id: String = entity.id.get
        val expectedId: String = entity.name.replace(' ', '_')
        if (id == expectedId) None else Some(s"- '$id' должен по идее называться '$expectedId'")
      }
    )

    writeReport(
      directory,
      name = "no-refs",
      title = "Имена без атрибута 'ref'",
      content =
        for (reference <- site.references.filter(_.value.ref.isEmpty)) yield
          "- " + reference.value.name.map(_.text.trim).mkString(" ") + " в " +
            referenceCollectionName(reference) + ":" +
            reference.path.last.store.names.name
    )
  }

  @scala.annotation.tailrec
  private def file(directory: File, segments: Seq[String]): File =
    if (segments.isEmpty) directory
    else file(new File(directory, segments.head), segments.tail)

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

  private def documentRef(collection: Collection, document: Document): Elem =
    ref(documentUrl(collection, document.name), document.name)

  private def translationRef(collection: Collection, teiHolder: TeiHolder): Elem =
    ref(documentUrl(collection, teiHolder.name), teiHolder.language.get)

  private def documentPageRef(collection: Collection, document: Document, page: Page): Elem =
    ref(documentUrl(collection, document.name) + s"#p${page.n}", page.displayName,
      Some(if (page.isPresent) "page" else "missing-page"))

  def ref(
    url: String,
    text: String,
    css: Option[String] = None
  ): Elem = <ref target={url} rendition={css.orNull}>{text}</ref>

  def refNg(
    url: Seq[String],
    text: String,
    css: Option[String] = None
  ): Elem = <ref target={mkUrl(url)} rendition={css.orNull}>{text}</ref>

  def ref(
    url: String,
    text: Seq[Node]
  ): Elem = <ref target={url}>{text}</ref>

  def refNg(
    url: Seq[String],
    text: Seq[Node]
  ): Elem = <ref target={mkUrl(url)}>{text}</ref>

  private def writeSiteObject(siteObject: SiteObject, directory: File): Unit = {
    val teiFile: TeiFile = siteObject.teiFile
    Files.write(file(directory, teiFile.url), teiFile.content)

    val teiWrapperFile: TeiWrapperFile = siteObject.teiWrapperFile
    // TODO separate generation of content and writing:
    writeWithYaml(
      file = file(directory, teiWrapperFile.url),
      layout = "tei",
      yaml =
        teiWrapperFile.style.fold[Seq[(String, String)]](Seq.empty)(style => Seq("style" -> style)) ++
        Seq(
          "tei" -> quote(mkUrl(teiFile.url)),
          "target" -> siteObject.viewer
        ) ++ teiWrapperFile.yaml
    )
  }

  def mkUrl(segments: Seq[String]): String = segments.mkString("/", "/", "")

  // TODO calculate viewer from the URL when writing Site.SiteObject:
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
          val roleShouldBe: Option[String] = site.resolve(target).map(_.siteObject.viewer)
          val role: Option[String] = elem.attribute("role").map(_.text)
          if (roleShouldBe.isEmpty) println(s"did not resolve: $target")
          if (roleShouldBe.isDefined && role.isDefined && (role != roleShouldBe)) println(s"role discrepancy")
          if ((role == roleShouldBe) || roleShouldBe.isEmpty || role.isDefined) elem
          else elem % scala.xml.Attribute(None, "role", textNode(roleShouldBe.get), scala.xml.Null)
        }
      }
    }

  def processTei(elem: Elem, site: Site): Elem =
    XmlUtil.rewriteElements(elem, refRoleRewriter(site))

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
