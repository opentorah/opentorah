package org.digitaljudaica.archive.collector

import java.io.File
import org.digitaljudaica.xml.{ContentType, Error, From, Parser, Xml, XmlUtil}
import org.digitaljudaica.archive.collector.reference.{Names, NamesListDescriptor}
import org.digitaljudaica.util.Files
import zio.{IO, ZIO}
import scala.xml.{Elem, Text}

object Main {

  def main(args: Array[String]): Unit = {
    val docs: File = new File(args(0))
    println(s"docs: $docs")
    val layout: Layout = new Layout(docs)

    val collections: Seq[Collection] = readCollections(layout)
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
    names.writeNames(layout.namesDirectory)
    names.writeList(
      directory = layout.namesFileDirectory,
      fileName = layout.namesFileName,
      namedInTheListUrl = layout.namedInTheListUrl
    )
  }

  private def readCollections(layout: Layout): Seq[Collection] = {
    val result: Seq[Collection] = for {
      directory <- layout.collections.listFiles.toSeq.filter(_.isDirectory)
    } yield Collection(layout, directory)

    //    println("Collections:")
    //    println(result.map { collection =>
    //      s"  ${collection.directoryName}: ${XmlUtil.spacedText(collection.title)}\n"
    //    }.mkString)

    result
  }

  private def processCollections(collections: Seq[Collection], layout: Layout): Unit = {
    println("Processing collections.")
    collections.foreach(_.process())

    println("Writing collection lists.")
    val collectionsSorted = collections.sorted
    writeCollectionsTree(collectionsSorted, layout)
    writeIndex(collectionsSorted, layout)
  }

  private def readNames(layout: Layout): Names = {
    println("Reading names.")

    val (listsHead: String, listDescriptors: Seq[NamesListDescriptor]) =
      NamesListDescriptor.readAll(layout.store, layout.namesListsFileName)

    new Names(
      reference = listsHead,
      teiNameds = readNameds(layout.storeNamesDirectory),
      listDescriptors,
      namedUrl = layout.namedUrl,
      namedInTheListUrl = layout.namedInTheListUrl
    )
  }

  // TODO Move into store
  private def readNameds(directory: File): Seq[org.digitaljudaica.reference.Named] = Parser.parseDo(collectAll(
    for {
      fileName <- Files.filesWithExtensions(directory, extension = "xml").sorted
    } yield From.file(directory, fileName)
      .parse(org.digitaljudaica.reference.Named.contentParser(fileName))
  ))

  // TODO Move into Parser
  def collectAll[A](parsers: Seq[Parser[A]]): Parser[Seq[A]] = for {
    runs <- ZIO.collectAll(parsers.map(_.either))
    errors: Seq[Error] = runs.flatMap(_.left.toOption)
    results: Seq[A] = runs.flatMap(_.right.toOption)
    results <- if (errors.nonEmpty) IO.fail(errors.mkString("Errors:\n  ", "\n  ", "\n.")) else IO.succeed(results)
  } yield results

  private def writeIndex(collections: Seq[Collection], layout: Layout): Unit = Util.writeTei(
    directory = layout.docs,
    fileName = layout.indexFileName,
    head = Some(Text("Дела")),
    content = <list type="bulleted">{for (collection <- collections.filter(_.publish)) yield toXml(collection, layout)}</list>,
    target = "collectionViewer",
    yaml = Seq("windowName" -> "collectionViewer")
  )

  private def writeCollectionsTree(collections: Seq[Collection], layout: Layout): Unit = {
    val byArchive: Map[String, Seq[Collection]] = collections.groupBy(_.archive.getOrElse(""))
    Util.writeTei(
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
}
