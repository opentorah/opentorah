package org.opentorah.archive.collector

import java.io.File
import org.opentorah.archive.collector.reference.{Named, Names, Reference}
import org.opentorah.tei.Tei
import org.opentorah.util.Files
import org.opentorah.xml.{From, Parser, Xml, XmlUtil}
import scala.xml.{Elem, Node, Text}

object Main {

  private val migrated: Boolean = false

  def main(args: Array[String]): Unit = {
    val docs: File = new File(args(0))
    println(s"docs: $docs")
    val layout: Layout = new Layout(docs)

    val collections: Seq[Collection] = for {
      directory <- layout.storeCollections.listFiles.toSeq.filter(_.isDirectory)
    } yield {
      // Read from 'docs/store/collections'
      val sourceDirectory: File =
        new File(if (migrated) layout.storeCollections else layout.collections, directory.getName)
      Parser.parseDo(
        From.file(directory, layout.collectionFileName).parse(Collection.parser(layout, sourceDirectory))
      )
    }

    // Write to 'docs/collections'
    for (collection <- collections; document <- collection.documents) Util.writeXml(
      layout.tei(new File(if (migrated) layout.collections else layout.storeCollections, collection.directoryName)),
      document.name,
      Tei.toXml(document.tei)
    )

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
      Util.writeXml(layout.storeNamesDirectory, named.id, Named.toXml(named))

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

      for (document <- collection.documents) document.writeWrappers(docsDirectory, facsDirectory)
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
          head <- Xml.text.required("head")
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
    head = Some(Text("Дела")),
    content = <list type="bulleted">{for (collection <- collections.filter(_.publish)) yield toXml(collection, layout)}</list>,
    target = "collectionViewer",
    yaml = Seq("windowName" -> "collectionViewer")
  )

  private def writeCollectionsTree(collections: Seq[Collection], layout: Layout): Unit = {
    val byArchive: Map[String, Seq[Collection]] = collections.groupBy(_.archive.getOrElse(""))
    writeTei(
      directory = layout.docs,
      fileName = layout.collectionsFileName,
      head = Some(Text("Архивы")),
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
      head = Some(Text(names.reference)),
      content = listOfLists ++ nonEmptyLists.flatMap(_.toXml),
      target = "namesViewer"
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

    Util.writeXml(directory, fileName, Tei.toXml(tei))

    Util.writeTeiWrapper(
      directory,
      fileName,
      teiPrefix = None,
      style,
      target,
      yaml = head.fold[Seq[(String, String)]](Seq.empty)(head => Seq("title" -> Util.quote(XmlUtil.spacedText(head)))) ++ yaml
    )
  }
}
