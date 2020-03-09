package org.opentorah.archive.collector

import java.io.File
import org.opentorah.archive.collector.reference.{Named, Names, Reference}
import org.opentorah.tei.Tei
import org.opentorah.util.Files
import org.opentorah.xml.{From, PaigesPrettyPrinter, Parser, Text, Xml, XmlUtil}
import scala.xml.{Elem, Node}

object Main {

  def main(args: Array[String]): Unit = {
    val docs: File = new File(args(0))
    println(s"docs: $docs")
    val layout: Layout = new Layout(docs)

    val collections: Seq[Collection] = for {
      directory <- layout.storeCollections.listFiles.toSeq.filter(_.isDirectory)
    } yield {
      // Read from 'docs/store/collections'
      val sourceDirectory: File =
        new File(layout.storeCollections, directory.getName)
      Parser.parseDo(
        From.file(directory, layout.collectionFileName).parse(Collection.parser(layout, sourceDirectory))
      )
    }

    for (collection <- collections; document <- collection.documents) {
      // Pretty-print to 'docs/store/collections'.
      // TODO do translations also!
      writeXml(
        layout.tei(new File(layout.storeCollections, collection.directoryName)),
        document.name,
        Tei.toXml(document.tei)
      )

      // Write to 'docs/collections'.
      // TODO do translations also!
      // TODO wipe out the directory first.
      // TODO remove repetitive TEI header components from the source document - and add them when writing the generated ones.
      writeXml(
        layout.tei(new File(layout.collections, collection.directoryName)),
        document.name,
        Tei.toXml(document.tei)
      )
    }

    //    println("Collections:")
    //    println(result.map { collection =>
    //      s"  ${collection.directoryName}: ${XmlUtil.spacedText(collection.title)}\n"
    //    }.mkString)

    processCollections(collections, layout)

    val names: Names = readNames(layout)

    println("Verifying names' ids.")
    for (named <- names.nameds) {
      val id = named.id
      val name = named.name
      val expectedId = name.replace(' ', '_')
      if (id != expectedId) println(s"id $id should be $expectedId")
    }

    println("Processing name references.")
    names.addDocumentReferences(collections.flatMap(_.references))
    names.checkReferences()

    writeNames(layout.namesDirectory, names.nameds, names.getReferences)

    println("Pretty-printing names")
    for (named <- names.nameds)
      writeXml(layout.storeNamesDirectory, named.id, Named.toXml(named))

    writeNamesList(
      names,
      directory = layout.namesFileDirectory,
      fileName = layout.namesFileName,
      namedInTheListUrl = layout.namedInTheListUrl
    )
  }

  private def processCollections(collections: Seq[Collection], layout: Layout): Unit = {
    println("Processing collections.")
    collections.foreach { collection =>
      val directory = new File(layout.collections, collection.directoryName)
      val docsDirectory = layout.docs(directory)
      val facsDirectory = layout.facs(directory)

      writeCollectionIndex(collection, directory, layout.documentUrlRelativeToIndex)
      // Wrappers
      Files.deleteFiles(docsDirectory)
      Files.deleteFiles(facsDirectory)

      for (document <- collection.documents) writeDocumentWrappers(
        document,
        docsDirectory,
        facsDirectory,
        layout.teiDirectoryName,
        layout.facsDirectoryName,
        layout.documentsDirectoryName
      )
    }

    println("Writing collection lists.")
    val collectionsSorted = collections.sorted
    writeCollectionsTree(collectionsSorted, layout)
    writeIndex(collectionsSorted, layout)
  }

  private def writeCollectionIndex(
    collection: Collection,
    directory: File,
    documentUrlRelativeToIndex: String => String
  ): Unit = writeTei(
    directory,
    fileName = "index",
    head = Some(collection.title),
    content = collection.description ++
      Seq[Elem](Collection.table(documentUrlRelativeToIndex).toTei(
        collection.parts.flatMap { part =>  part.title.map(Table.Xml).toSeq ++ part.documents.map(Table.Data[Document]) }
      )) ++
      (if (collection.missingPages.isEmpty) Seq.empty
      else Seq(<p>Отсутствуют фотографии {collection.missingPages.length} страниц: {collection.missingPages.mkString(" ")}</p>)),
    style = Some("wide"),
    target = "collectionViewer",
    yaml = Seq("documentCollection" -> Util.quote(collection.reference))
  )

  private def readNames(layout: Layout): Names = {
    println("Reading names.")

    val (listsHead: String, storeNamesLists: Seq[org.opentorah.reference.NamesList]) =
      Parser.parseDo(From.file(layout.store, layout.namesListsFileName)
        .parse(Xml.withName("names", for {
          head <- Text("head").required
          listDescriptors <- org.opentorah.reference.NamesList.all
        } yield (head, listDescriptors))))

    new Names(
      reference = listsHead,
      storeNameds = org.opentorah.reference.Named.readAll(layout.storeNamesDirectory),
      storeNamesLists,
      namedUrl = layout.namedUrl,
      namedInTheListUrl = layout.namedInTheListUrl
    )
  }

  private def writeNames(
    directory: File,
    nameds: Seq[Named],
    references: Seq[Reference]
  ): Unit = {
    println("Writing names")
    Files.deleteFiles(directory)
    for (named <- nameds) writeTei(
      directory,
      fileName = named.id,
      head = None,
      content = Seq(named.toXml(references)),
      target = "namesViewer"
    )
  }

  private def writeIndex(collections: Seq[Collection], layout: Layout): Unit = writeTei(
    directory = layout.docs,
    fileName = layout.indexFileName,
    head = Some(scala.xml.Text("Дела")),
    content = <list type="bulleted">{for (collection <- collections.filter(_.publish)) yield toXml(collection, layout)}</list>,
    target = "collectionViewer",
    yaml = Seq("windowName" -> "collectionViewer")
  )

  private def writeCollectionsTree(collections: Seq[Collection], layout: Layout): Unit = {
    val byArchive: Map[String, Seq[Collection]] = collections.groupBy(_.archive.getOrElse(""))
    writeTei(
      directory = layout.docs,
      fileName = layout.collectionsFileName,
      head = Some(scala.xml.Text("Архивы")),
      content = <list>{
        for (archive <- byArchive.keys.toList.sorted) yield {
          <item>
            <p>{s"[$archive]"}</p>
            <list type="bulleted">{for (collection <- byArchive(archive)) yield toXml(collection, layout)}</list>
          </item>}}
      </list>,
      target = "collectionViewer"
    )
  }

  private def toXml(collection: Collection, layout: Layout): Elem =
    <item>
      <ref target={layout.collectionUrl(collection.directoryName)}
           role="collectionViewer">{collection.reference + ": " + XmlUtil.spacedText(collection.title)}</ref>
      <lb/>
      <abstract>{collection.caseAbstract}</abstract>
    </item>

  private def writeNamesList(
    names: Names,
    directory: File,
    fileName: String,
    namedInTheListUrl: String => String
  ): Unit = {
    // List of all names
    val nonEmptyLists = names.lists.filterNot(_.isEmpty)

    val listOfLists: Seq[Node] =
      <p>{for (list <- nonEmptyLists) yield
        <l>{<ref target={namedInTheListUrl(list.id)} role="namesViewer">{list.head}</ref>}</l>
        }</p>

    writeTei(
      directory = directory,
      fileName = fileName,
      head = Some(scala.xml.Text(names.reference)),
      content = listOfLists ++ nonEmptyLists.flatMap(_.toXml),
      target = "namesViewer"
    )
  }

  def writeDocumentWrappers(
    document: Document,
    docsDirectory: File,
    facsDirectory: File,
    teiDirectoryName: String,
    facsDirectoryName: String,
    documentsDirectoryName: String
  ): Unit = {
    import Util.quote
    val navigation: Seq[(String, String)] =
      Seq("documentCollection" -> quote(document.collection.reference)) ++
        document.prev.map(prev => Seq("prevDocument" -> quote(prev))).getOrElse(Seq.empty) ++
        Seq("thisDocument" -> quote(document.name)) ++
        document.next.map(next => Seq("nextDocument" -> quote(next))).getOrElse(Seq.empty)

    def writeTeiWrapper(name: String, lang: Option[String]): Unit = {
      val nameWithLang: String = lang.fold(name)(lang => name + "-" + lang)

      Util.writeTeiWrapper(
        directory = docsDirectory,
        fileName = nameWithLang,
        teiPrefix = Some(s"../$teiDirectoryName/"),
        target = "documentViewer",
        yaml = Seq(
          "facs" -> s"'../$facsDirectoryName/$name.html'"
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
            <a target="documentViewer" href={s"../$documentsDirectoryName/${document.name}.html#p$n"}>
              <figure>
                <img xml:id={s"p$n"} alt={s"facsimile for page $n"} src={page.facs.orNull}/>
                <figcaption>{n}</figcaption>
              </figure>
            </a>}}
        </div>
      </div>

    Util.writeWithYaml(
      file = Util.htmlFile(facsDirectory, document.name),
      layout = "default",
      yaml = Seq(
        "transcript" -> s"'../$documentsDirectoryName/${document.name}.html'"
      )
        ++ navigation,
      content = Seq(render(facsimilePages) + "\n")
    )
  }

  private def writeTei(
    directory: File,
    fileName: String,
    head: Option[Node], // TODO do not supply where not needed
    content: Seq[Node],
    style: Option[String] = None,
    target: String,
    yaml: Seq[(String, String)] = Seq.empty
  ): Unit = {
    val tei = Tei(
      publisher = <ptr target="www.alter-rebbe.org"/>,
      availabilityStatus = "free", availability =
        <licence>
          <ab>
            <ref n="license" target="http://creativecommons.org/licenses/by/4.0/">
              Creative Commons Attribution 4.0 International License</ref>
          </ab>
        </licence>,
      sourceDesc = <p>Facsimile</p>,
      body = head.fold[Seq[Node]](Seq.empty)(head => Seq(<head>{head}</head>)) ++ content
    )

    writeXml(directory, fileName, Tei.toXml(tei))

    Util.writeTeiWrapper(
      directory,
      fileName,
      teiPrefix = None,
      style,
      target,
      yaml = head.fold[Seq[(String, String)]](Seq.empty)(head => Seq("title" -> Util.quote(XmlUtil.spacedText(head)))) ++ yaml
    )
  }

  private def writeXml(
    directory: File,
    fileName: String,
    elem: Elem
  ): Unit = Files.write(
    file = new File(directory, fileName + ".xml"),
    content = """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" + render(elem) + "\n"
  )

  private def render(elem: Elem): String = new PaigesPrettyPrinter(
    width = 120,
    indent = 2,
    doNotStackElements = Set("choice"),
    nestElements = Set("p", /*"abstract",*/ "head", "salute", "dateline", "item"),
    clingyElements = Set("note", "lb", "sic", "corr") // TODO unclingify sic and corr?
  ).render(elem)
}
