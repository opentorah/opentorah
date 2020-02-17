package org.digitaljudaica.archive.collector

import java.io.File
import org.digitaljudaica.xml.Print
import org.digitaljudaica.archive.collector.reference.Names
import scala.xml.{Elem, Text}

object Main {

  def main(args: Array[String]): Unit = {
    val docs: File = new File(args(0))
    println(s"docs: $docs")
    val layout: Layout = new Layout(docs)

    val collections: Seq[Collection] = for {
      directory <- layout.collections.listFiles.toSeq.filter(_.isDirectory)
    } yield Collection(layout, directory)

    println("Collections:")
    println(collections.map { collection =>
      s"  ${collection.directoryName}: ${Print.spacedText(collection.title)}\n"
    }.mkString)

    println("Processing collections.")
    collections.foreach(_.process())

    println("Writing collection lists.")
    val collectionsSorted = collections.sorted
    writeCollectionsTree(collectionsSorted, layout)
    writeIndex(collectionsSorted, layout)

    println("Processing name references.")
    val names: Names = Names(layout.namesDirectory, layout)
    names.processReferences(collections.flatMap(_.references))
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
            <p>[{archive}]</p>
            <list type="bulleted">{for (collection <- byArchive(archive)) yield toXml(collection, layout)}</list>
          </item>}}
      </list>,
      target = "collectionViewer"
    )
  }

  private def toXml(collection: Collection, layout: Layout): Elem =
    <item>
      <ref target={layout.collectionUrl(collection.directoryName)}
           role="collectionViewer">{collection.reference + ": " + Print.spacedText(collection.title)}</ref>
      <lb/>
      <abstract>{collection.caseAbstract}</abstract>
    </item>
}
