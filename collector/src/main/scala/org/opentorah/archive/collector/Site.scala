package org.opentorah.archive.collector

import java.io.File
import org.opentorah.metadata.Language
import org.opentorah.reference.{Named, NamedsList, Reference}
import org.opentorah.store.Selector
import org.opentorah.tei.Tei
import org.opentorah.util.{Collections, Files}
import org.opentorah.xml.XmlUtil
import scala.xml.{Elem, Node}

final class Site(directory: File) {
  val collectionsDirectory: File = new File(directory, Site.collectionsDirectoryName)

  val namesDirectory: File = new File(directory, Site.namesDirectoryName)

  def reportFile(name: String) = new File(new File(directory, "reports"), name + ".md")

  def write(
    namesSelector: Selector.Nullary,
    lists: Seq[NamedsList],
    nameds: Seq[Named],
    caseSelector: Selector.Named,
    collections: Seq[Collection],
    references: Seq[Reference]
  ): Unit = {
    writeNames(
      namesSelector,
      caseSelector,
      nameds,
      references
    )

    writeNamesList(
      namesSelector.names.doFind(Language.Russian.toSpec).name,
      lists
    )

    writeDocuments(collections)

    processCollections(collections)

    val collectionsSorted = collections.sorted
    writeCollectionsTree(collectionsSorted)
    writeIndex(collectionsSorted)
  }

  def processCollections(collections: Seq[Collection]): Unit = {
    println("Processing collections.")
    collections.foreach { collection =>
      val directory = new File(collectionsDirectory, collection.name)

      writeCollectionIndex(collection, directory)
      // Wrappers
      Files.deleteFiles(Site.docs(directory))
      Files.deleteFiles(Site.facs(directory))

      val documents: Seq[(Document, (Option[Document], Option[Document]))] = Collections.prevAndNext(collection.documents)
      for ((document, (prev, next)) <- documents) writeDocumentWrappers(
        directory,
        collection,
        document,
        prev.map(_.name),
        next.map(_.name)
      )
    }
  }

  private def writeCollectionIndex(
    collection: Collection,
    directory: File
  ): Unit = {
    Util.writeTei(
      directory,
      fileName = "index",
      head = Some(collection.title),
      content = collection.description ++
        Seq[Elem](table(Site.documentUrlRelativeToIndex).toTei(
          collection.parts.flatMap { part =>  part.title.map(Table.Xml).toSeq ++ part.documents.map(Table.Data[Document]) }
        )) ++
        (if (collection.missingPages.isEmpty) Seq.empty
        else Seq(<p>Отсутствуют фотографии {collection.missingPages.length} страниц: {collection.missingPages.mkString(" ")}</p>)),
      style = Some("wide"),
      target = "collectionViewer",
      yaml = Seq("documentCollection" -> Util.quote(collection.reference))
    )
  }

  private def table(documentUrlRelativeToIndex: String => String): Table[Document] = new Table[Document](
    Table.Column("Описание", "description", { document: Document =>
      document.description.getOrElse(Seq.empty).map(XmlUtil.removeNamespace)
    }),

    Table.Column("Дата", "date", { document: Document =>
      document.date.fold[Seq[Node]](scala.xml.Text(""))(value => scala.xml.Text(value))
    }),

    Table.Column("Кто", "author", { document: Document =>
      multi(document.authors.flatMap(_.map(XmlUtil.removeNamespace)))
    }),

    Table.Column("Кому", "addressee",  _.addressee.fold[Seq[Node]](scala.xml.Text(""))(addressee =>
      <persName ref={addressee.ref.orNull}>{addressee.name}</persName>)),

    Table.Column("Язык", "language", { document: Document =>
      val translations: Seq[Elem] = for (translation <- document.translations) yield
        <ref target={documentUrlRelativeToIndex(document.name + "-" + translation)}
             role="documentViewer">{translation}</ref>

      Seq(scala.xml.Text(document.language.getOrElse("?"))) ++ translations
    }),

    Table.Column("Документ", "document", { document: Document =>
      <ref target={documentUrlRelativeToIndex(document.name)}
           role="documentViewer">{document.name}</ref>
    }),

    Table.Column("Страницы", "pages", { document: Document => for (page <- document.pages) yield
      <ref target={documentUrlRelativeToIndex(document.name) + s"#p${page.n}"}
           role="documentViewer"
           rendition={if (page.isPresent) "page" else "missing-page"}>{page.displayName}</ref>
    }),

    Table.Column("Расшифровка", "transcriber", { document: Document =>
      multi(document.transcribers.map(transcriber => XmlUtil.removeNamespace(org.opentorah.reference.Reference.toXml(transcriber))))
    })
  )

  private def multi(nodes: Seq[Node]): Seq[Node] = nodes match {
    case Nil => Nil
    case n :: Nil => Seq(n)
    case n :: ns if n.isInstanceOf[Elem] => Seq(n, scala.xml.Text(", ")) ++ multi(ns)
    case n :: ns => Seq(n) ++ multi(ns)
    case n => n
  }

  def writeDocuments(collections: Seq[Collection]): Unit = {
    // TODO do translations also!
    // TODO wipe out the directory first.
    // TODO remove repetitive TEI header components from the source document - and add them when writing the generated ones.
    for (collection <- collections) {
      val directory = new File(new File(collectionsDirectory, collection.name), Site.teiDirectoryName)
      for (document <- collection.documents) Util.writeXml(
        directory,
        document.name,
        Tei.toXml(document.tei)
      )
    }
  }

  def writeDocumentWrappers(
    directory: File,
    collection: Collection,
    document: Document,
    prevName: Option[String],
    nextName: Option[String]
  ): Unit = {

    import Util.quote
    val navigation: Seq[(String, String)] =
      Seq("documentCollection" -> quote(collection.reference)) ++
        prevName.map(prev => Seq("prevDocument" -> quote(prev))).getOrElse(Seq.empty) ++
        Seq("thisDocument" -> quote(document.name)) ++
        nextName.map(next => Seq("nextDocument" -> quote(next))).getOrElse(Seq.empty)

    def writeTeiWrapper(name: String, lang: Option[String]): Unit = {
      val nameWithLang: String = lang.fold(name)(lang => name + "-" + lang)

      Util.writeTeiWrapper(
        directory = Site.docs(directory),
        fileName = nameWithLang,
        teiPrefix = Some(s"../${Site.teiDirectoryName}/"),
        target = "documentViewer",
        yaml = Seq(
          "facs" -> s"'../${Site.facsDirectoryName}/$name.html'"
        ) ++ (
          if (lang.isDefined || document.translations.isEmpty) Seq.empty
          else Seq("translations" -> document.translations.mkString("[", ", ", "]")))
          ++ navigation
      )
    }

    // TEI wrapper(s)
    writeTeiWrapper(document.name, None)
    for (lang <- document.translations) writeTeiWrapper(document.name, Some(lang))

    // Facsimile viewer
    val facsimilePages: Elem =
      <div class="facsimileViewer">
        <div class="facsimileScroller">{
          for (page: Page <- document.pages.filter(_.isPresent); n = page.n) yield {
            <a target="documentViewer" href={s"../${Site.documentsDirectoryName}/${document.name}.html#p$n"}>
              <figure>
                <img xml:id={s"p$n"} alt={s"facsimile for page $n"} src={page.facs.orNull}/>
                <figcaption>{n}</figcaption>
              </figure>
            </a>}}
        </div>
      </div>

    Util.writeWithYaml(
      file = Util.htmlFile(Site.facs(directory), document.name),
      layout = "default",
      yaml = Seq(
        "transcript" -> s"'../${Site.documentsDirectoryName}/${document.name}.html'"
      )
        ++ navigation,
      content = Seq(Util.render(facsimilePages) + "\n")
    )
  }

  def writeIndex(collections: Seq[Collection]): Unit = Util.writeTei(
    directory,
    fileName = Site.indexFileName,
    head = Some(scala.xml.Text("Дела")),
    content = <list type="bulleted">{for (collection <- collections.filter(_.publish)) yield ToXml.toXml(collection)}</list>,
    target = "collectionViewer",
    yaml = Seq("windowName" -> "collectionViewer")
  )

  def writeCollectionsTree(collections: Seq[Collection]): Unit = {
    val byArchive: Map[String, Seq[Collection]] = collections.groupBy(_.archive.getOrElse(""))
    Util.writeTei(
      directory,
      fileName = Site.collectionsFileName,
      head = Some(scala.xml.Text("Архивы")),
      content = <list>{
        for (archive <- byArchive.keys.toList.sorted) yield {
          <item>
            <p>{s"[$archive]"}</p>
            <list type="bulleted">{for (collection <- byArchive(archive)) yield ToXml.toXml(collection)}</list>
          </item>}}
      </list>,
      target = "collectionViewer"
    )
  }

  def writeNames(
    namesSelector: Selector.Nullary,
    caseSelector: Selector.Named,
    nameds: Seq[Named],
    references: Seq[Reference]
  ): Unit = {
    val directory: File = new File(this.directory, Site.namesDirectoryName)

    Files.deleteFiles(directory)

    for (named <- nameds) Util.writeTei(
      directory,
      fileName = named.id.get,
      head = None,
      content = Seq(ToXml.toXml(
        namesSelector,
        caseSelector,
        named,
        references
      )),
      target = "namesViewer"
    )
  }

  def writeNamesList(namesHead: String, lists: Seq[NamedsList]): Unit = {
    // List of all names
    val nonEmptyLists = lists.filterNot(_.isEmpty)

    val listOfLists: Seq[Node] =
      <p>{for (list <- nonEmptyLists) yield
        <l>{<ref target={Site.namedInTheListUrl(list.id)} role="namesViewer">{list.head}</ref>}</l>
        }</p>

    Util.writeTei(
      directory = directory,
      fileName = Site.namesFileName,
      head = Some(scala.xml.Text(namesHead)),
      content = listOfLists ++ nonEmptyLists.flatMap(ToXml.toXml),
      target = "namesViewer"
    )
  }

  def writeReport(name: String, title: String, content: Seq[String]): Unit = Util.writeWithYaml(
    file = reportFile(name),
    layout = "page",
    yaml = Seq("title" -> title),
    content :+ "\n"
  )
}

object Site {
  val indexFileName: String = "index"

  val collectionsFileName: String = "collections"

  // TODO Note: also hard-coded in 'index.xml'!
  val collectionsDirectoryName: String = "collections"

  val namesDirectoryName: String = "names"

  val namesFileName: String = "names"

  // TODO Note: also hard-coded in _layouts/tei.html!
  val facsDirectoryName: String = "facs" // facsimile viewers

  val documentsDirectoryName: String = "documents"

  val teiDirectoryName: String = "tei"

  def facs(collectionDirectory: File): File = new File(collectionDirectory, facsDirectoryName)

  def namedUrl(id: String): String = s"/$namesDirectoryName/$id.html"

  def namedInTheListUrl(id: String): String = s"/$namesFileName.html#$id"

  def documentUrl(collectionDirectoryName: String, name: String): String =
    url(s"$collectionDirectoryName/${documentUrlRelativeToIndex(name)}")

  def documentUrlRelativeToIndex(name: String): String =  s"$docsDirectoryName/$name.html"

  def docs(collectionDirectory: File): File = new File(collectionDirectory, docsDirectoryName)

  val docsDirectoryName: String = "documents" // wrappers for TEI XML

  def collectionUrl(collectionName: String): String = url(s"$collectionName/index.html")

  def url(ref: String): String = s"/$collectionsDirectoryName/$ref"
}
