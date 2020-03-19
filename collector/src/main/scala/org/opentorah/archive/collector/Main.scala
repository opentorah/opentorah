package org.opentorah.archive.collector

import java.io.File
import org.opentorah.metadata.Language
import org.opentorah.reference.{Named, NamesList, Reference}
import org.opentorah.store.{Binding, Path}
import org.opentorah.tei.Tei
import org.opentorah.util.Files
import org.opentorah.xml.{Element, From, PaigesPrettyPrinter, Parser, Text, XmlUtil}
import scala.xml.{Elem, Node}

object Main {

  def main(args: Array[String]): Unit = {
    val docs: File = new File(args(0))
    println(s"docs: $docs")
    val layout: Layout = new Layout(docs)

    val collectionNames: Seq[String] = Parser.parseDo(
      new Element("collections", parser = Text("collection").allMustBe)
        .parse(From.file(layout.store, "collections")))

    val collections: Seq[Collection] = for (collectionName <- collectionNames) yield {
      Parser.parseDo(Collection.parse(
        collectionName,
        from = From.file(layout.storeCollections, collectionName)
      ))
    }

    for (collection <- collections; document <- collection.documents) {
      // Pretty-print TEI files.
      // TODO do translations also!
      writeXml(
        Files.toFile(document.url),
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

    reportMisnamedNameds(names, layout.reportFile("misnamed-nameds"))

    println("Processing name references.")
    names.addDocumentReferences(collections.flatMap(_.references))

    checkReferences(names)
    reportReferencesWithoutRef(names, layout.reportFile("no-refs"))

    writeNames(layout, layout.namesDirectory, names.nameds, names.getReferences)

    println("Pretty-printing names")
    for (named <- names.nameds)
      writeXml(layout.storeNamesDirectory, named.id.get, Named.toXml(named.copy(id = None)))

    writeNamesList(
      names,
      directory = layout.namesFileDirectory,
      fileName = Layout.namesFileName,
      namedUrl = layout.namedUrl,
      namedInTheListUrl = layout.namedInTheListUrl
    )
  }

  def reportMisnamedNameds(names: Names, file: File): Unit = {
    val content: Seq[Option[String]] =
      for (named <- names.nameds) yield {
        val id = named.id
        val name = named.name
        val expectedId = name.replace(' ', '_')
        if (id.get == expectedId) None else Some(s"- '${id.get}' должен по идее называться '$expectedId'")
      }

    Util.writeWithYaml(
      file,
      layout = "page",
      yaml = Seq("title" -> "Неправильно названные файлы с именами"),
      content.flatten :+ "\n"
    )
  }

  def checkReferences(names: Names): Unit = {
    def check(reference: Reference): Option[String] = {
      val name = reference.name
      reference.ref.fold[Option[String]](None) { ref =>
        if (ref.contains(" ")) Some(s"""Value of the ref attribute contains spaces: ref="$ref" """) else {
          names.findByRef(ref).fold[Option[String]](Some(s"""Unresolvable reference: Name ref="$ref">${name.text}< """)) { named =>
            if (named.entity != reference.entity) Some(s"${reference.entity} reference to ${named.entity} ${named.name}: $name [$ref]")
            else None
          }
        }
      }
    }

    val errors: Seq[String] = names.getReferences.flatMap(check)
    if (errors.nonEmpty) throw new IllegalArgumentException(errors.mkString("\n"))
  }

  def reportReferencesWithoutRef(names: Names, file: File): Unit = {
    val content: Seq[String] =
      for (reference <- names.getReferences.filter(_.ref.isEmpty))
      yield {
        val in: String = reference.source.init.reference(Language.Russian.toSpec) + ":" +
          reference.source.reference(Language.Russian.toSpec)
        "- " + reference.name.map(_.text.trim).mkString(" ") + s" в $in"
      }

    Util.writeWithYaml(
      file,
      layout = "page",
      yaml = Seq("title" -> "Имена без атрибута 'ref'"),
      content :+ "\n"
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
        Layout.teiDirectoryName,
        Layout.facsDirectoryName,
        Layout.documentsDirectoryName
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
    yaml = Seq("documentCollection" -> Util.quote(collection.path.reference(Language.Russian.toSpec)))
  )

  private def readNames(layout: Layout): Names = {
    println("Reading names.")

    val parsable = new Element[(String, Seq[NamesList])](
      elementName = "names",
      parser = for {
        head <- Text("head").required
        listDescriptors <- NamesList.all
      } yield (head, listDescriptors)
    )

    val (listsHead: String, storeNamesLists: Seq[NamesList]) =
      Parser.parseDo(parsable.parse(From.file(layout.store, Layout.namesListsFileName)))

    new Names(
      path = new Path(Seq(Binding.Nullary(Selectors.Names))),
      storeNameds = Named.readAll(layout.storeNamesDirectory),
      storeNamesLists
    )
  }

  private def writeNames(
    layout: Layout,
    directory: File,
    nameds: Seq[Named],
    references: Seq[Reference]
  ): Unit = {
    println("Writing names")
    Files.deleteFiles(directory)
    for (named <- nameds) writeTei(
      directory,
      fileName = named.id.get,
      head = None,
      content = Seq(ToXml.toXml(named, references, layout.namedUrl, layout.namedInTheListUrl, layout.documentUrl)),
      target = "namesViewer"
    )
  }

  private def writeIndex(collections: Seq[Collection], layout: Layout): Unit = writeTei(
    directory = layout.docs,
    fileName = Layout.indexFileName,
    head = Some(scala.xml.Text("Дела")),
    content = <list type="bulleted">{for (collection <- collections.filter(_.publish)) yield toXml(collection, layout)}</list>,
    target = "collectionViewer",
    yaml = Seq("windowName" -> "collectionViewer")
  )

  private def writeCollectionsTree(collections: Seq[Collection], layout: Layout): Unit = {
    val byArchive: Map[String, Seq[Collection]] = collections.groupBy(_.archive.getOrElse(""))
    writeTei(
      directory = layout.docs,
      fileName = Layout.collectionsFileName,
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
           role="collectionViewer">{collection.path.reference(Language.Russian.toSpec) + ": " +
        XmlUtil.spacedText(collection.title)}</ref>
      <lb/>
      <abstract>{collection.caseAbstract.xml}</abstract>
    </item>

  private def writeNamesList(
    names: Names,
    directory: File,
    fileName: String,
    namedUrl: String => String,
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
      head = Some(scala.xml.Text(names.path.reference(Language.Russian.toSpec))),
      content = listOfLists ++ nonEmptyLists.flatMap(ToXml.toXml(_, namedUrl)),
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
      Seq("documentCollection" -> quote(document.path.init.reference(Language.Russian.toSpec))) ++
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
  ): Unit = writeXml(
    file = new File(directory, fileName + ".xml"),
    elem
  )

  private def writeXml(
    file: File,
    elem: Elem
  ): Unit = Files.write(
    file,
    content = """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" + render(elem) + "\n"
  )

  private def render(elem: Elem): String = new PaigesPrettyPrinter(
    width = 120,
    indent = 2,
    doNotStackElements = Set("choice"),
    nestElements = Set("p", /*"abstract",*/ "head", "salute", "dateline", "item"),
    clingyElements = Set("note", "lb", "sic", "corr")
  ).render(elem)
}
