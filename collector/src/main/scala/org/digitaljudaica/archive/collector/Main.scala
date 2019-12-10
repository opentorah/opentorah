package org.digitaljudaica.archive.collector

import java.io.File

import scala.xml.{Elem, Text}

object Main {

  def main(args: Array[String]): Unit = {
    val docs: File = new File(args(0))
    println(s"docs: $docs")
    val layout: Layout = new Layout(docs)

    val collections: Seq[Collection] = for {
      directory <- layout.collections.listFiles.toSeq.filter(_.isDirectory)
    } yield new Collection(
      layout,
      directory,
      Xml.load(directory, layout.collectionFileName)
    )

    println("Collections:")
    println(collections.map { collection =>
      s"  ${collection.directoryName}: ${Xml.spacedText(collection.title)}\n"
    }.mkString)

    println("Processing collections.")
    collections.foreach(_.process())

    // Collection lists
    val collectionsSorted = collections.sorted
    writeCollectionsTree(collectionsSorted, layout)
    writeIndex(collectionsSorted, layout)

    println("Processing name references.")
    val names: Names = new Names(layout.namesDirectory, layout)
    names.processReferences(collections.flatMap(_.references))
  }

  private def writeIndex(collections: Seq[Collection], layout: Layout): Unit = {
    val byArchive: Map[String, Seq[Collection]] = collections.filter(_.publish).groupBy(_.archive.getOrElse(""))
    val collectionsContent: Elem =
      <list type="bulleted">{
        for (collection <- collections) yield
          <item>
            <ref target={layout.collectionUrl(collection.directoryName)}
                 role="collectionViewer">{collection.reference}</ref>: {collection.title}
            {collection.caseAbstract}
          </item>}
      </list>

    Util.writeTei(
      directory = layout.docs,
      fileName = layout.indexFileName,
      head = Some(Text("Дела")),
      content = collectionsContent,
      target = "collectionViewer",
      yaml = Seq("windowName" -> "collectionViewer")
    )
  }

  private def writeCollectionsTree(collections: Seq[Collection], layout: Layout): Unit = {
    val byArchive: Map[String, Seq[Collection]] = collections.groupBy(_.archive.getOrElse(""))
    val collectionsContent: Elem =
      <list>{
        for (archive <- byArchive.keys.toList.sorted) yield {
          <item>
            <p>[{archive}]</p>
            <list type="bulleted">{
              for (collection <- byArchive(archive)) yield
                <item>
                  <ref target={layout.collectionUrl(collection.directoryName)}
                       role="collectionViewer">{collection.archiveCase}</ref>: {collection.title}
                  {collection.caseAbstract}
                </item>}
            </list>
          </item>}}
      </list>

    Util.writeTei(
      directory = layout.docs,
      fileName = layout.collectionsFileName,
      head = Some(Text("Архивы")),
      content = collectionsContent,
      target = "collectionViewer"
    )
  }
}
