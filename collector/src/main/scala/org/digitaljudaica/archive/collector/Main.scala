package org.digitaljudaica.archive.collector

import java.io.File

import scala.xml.Elem
import Xml.Ops

object Main {

  def main(args: Array[String]): Unit = {
    val docs: File = new File(args(0))
    println(s"docs: $docs")
    val layout: Layout = new Layout(docs)
    val errors: Errors = new Errors

    val collections: Seq[Collection] = for {
      directory <- layout.collections.listFiles.filter(_.isDirectory)
    } yield new Collection(
      layout,
      directory,
      Xml.load(directory, layout.collectionFileName),
      errors
    )

    println("Collections:")
    println(collections.map { collection =>
      s"  ${collection.directoryName}: ${collection.title}\n"
    }.mkString)

    errors.check()

    println("Processing collections.")
    collections.foreach(_.process())

    errors.check()

    val byArchive: Map[String, Seq[Collection]] = collections.groupBy(_.archive)
    val collectionLinks: Seq[Seq[String]] = for (archive <- byArchive.keys.toList.sorted) yield {
      val cases: Seq[Seq[String]] = for (collection <- byArchive(archive).sortBy(_.archiveCase)) yield Seq(
        s"""  - <a href="${layout.collectionUrl(collection.directoryName)}" target="collectionViewer">${collection.archiveCase}</a>:""",
        s"  ${collection.title}"
      )
      s"- [$archive]" +: cases.flatten
    }

    Util.writeWithYaml(layout.collectionsMd, "page", Seq(
      "title" -> "Дела", "target" -> "collectionViewer"
    ), collectionLinks.flatten)

    println("Processing name references.")
    val names: Names = new Names(layout, errors)
    names.processReferences(collections.flatMap(_.references))

    errors.check()
  }
}
