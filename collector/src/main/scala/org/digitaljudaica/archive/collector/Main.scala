package org.digitaljudaica.archive.collector

import java.io.File

import scala.xml.{Elem, Text}

object Main {

  def main(args: Array[String]): Unit = {
    val docs: File = new File(args(0))
    println(s"docs: $docs")
    val layout: Layout = new Layout(docs)
    val errors: Errors = new Errors

    val collections: Seq[Collection] = for {
      directory <- layout.collections.listFiles.toSeq.filter(_.isDirectory)
    } yield new Collection(
      layout,
      directory,
      Xml.load(directory, layout.collectionFileName),
      errors
    )

    println("Collections:")
    println(collections.map { collection =>
      s"  ${collection.directoryName}: ${Xml.spacedText(collection.title)}\n"
    }.mkString)

    errors.check()

    println("Processing collections.")
    collections.foreach(_.process())

    errors.check()

    val byArchive: Map[String, Seq[Collection]] = collections.sorted.groupBy(_.archive.getOrElse(""))
    val collectionsContent: Elem =
      <list>{
        for (archive <- byArchive.keys.toList.sorted) yield {
          <item>
            <p>[{archive}]</p>
            <list>{
              for (collection <- byArchive(archive)) yield
                <item>
                  <ref target={layout.collectionUrl(collection.directoryName)}
                       role="collectionViewer">{collection.archiveCase}</ref>: {collection.title}
                </item>}
            </list>
          </item>}}
      </list>

    Util.writeTei(
      directory = layout.docs,
      fileName = layout.collectionsFileName,
      head = Text("Дела"),
      content = collectionsContent,
      target = "collectionViewer"
    )

    println("Processing name references.")
    val names: Names = new Names(layout, errors)
    names.processReferences(collections.flatMap(_.references))

    errors.check()
  }
}
