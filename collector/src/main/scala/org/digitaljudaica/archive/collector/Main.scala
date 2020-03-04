package org.digitaljudaica.archive.collector

import java.io.File

import org.digitaljudaica.xml.XmlUtil
import org.digitaljudaica.archive.collector.reference.{Named, Names, Reference}
import org.digitaljudaica.util.Files
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

    writeNames(
      layout.namesDirectory,
      names.nameds,
      names.getReferences
    )

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

    val (listsHead: String, storeNamesLists: Seq[org.digitaljudaica.reference.NamesList]) =
      org.digitaljudaica.reference.NamesList.readAll(layout.store, layout.namesListsFileName)

    new Names(
      reference = listsHead,
      storeNameds = org.digitaljudaica.reference.Named.readAll(layout.storeNamesDirectory),
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
    for (named <- nameds) Util.writeTei(
      directory,
      fileName = named.id,
      head = None,
      content = Seq(named.toXml(references)),
      target = "namesViewer"
    )
  }

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
