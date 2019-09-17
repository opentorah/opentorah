package org.digitaljudaica.archive.collector

import java.io.File

import scala.xml.Elem
import Xml.Ops

object Main {

  def main(args: Array[String]): Unit = {
    val docs: File = new File(args(0))
    println(s"docs: $docs")
    val layout: Layout = new Layout(docs)

    val collections: Seq[Collection] = getCollections(layout)

    println("Collections:")
    println(collections.map { collection =>
      s"  ${collection.directoryName}: ${collection.title} [${collection.includeInNavigation}]\n"
    }.mkString)

    println("Processing collections.")
    collections.foreach(_.process())

    Util.splice(
      file = layout.indexMd,
      start = """<a name="collections-start">""",
      end = """<a name="collections-end">""",
      what = collections.flatMap { collection => Seq(
        s"""- <a href="${layout.collectionUrl(collection.directoryName)}" target="collectionViewer">${collection.reference}</a>:""",
        s"${collection.title}."
      )}
    )

    Util.splice(
      file = layout.configYml,
      start = "# collections-start",
      end = "# collections-end",
      what = collections.filter(_.includeInNavigation).map { collection =>
        val url: String = layout.collectionUrl(collection.directoryName)
        // Links with starting slash do not make it into the navigation bar?
        val ref: String = if (url.startsWith("/")) url.substring(1) else url
        s"  - $ref"
      }
    )

    println("Processing name references.")
    val report: Option[Seq[String]] = new Names(layout).processReferences(documentReferences = collections.flatMap(_.names))

    report.foreach(report =>
      throw new IllegalArgumentException("\nTEI validation failed!\n" + report.mkString("\n")))
  }

  // Collections listed in collections.xml in the order they are listed there -
  // or directories with collection.xml in alphabetical order.
  private def getCollections(layout: Layout): Seq[Collection] = {
    val result: Seq[Collection] = for {
      xml: Elem <- Xml
        .load(layout.collectionsXml)
        .check(name = "collections")
        .elems(name = "collection")
      name: String = xml.text
      directory: File = layout.collections(name)
    } yield new Collection(
      layout,
      directory,
      Xml.load(layout.collectionXml(directory)),
      includeInNavigation = xml.attributeOption("includeInNavigation").contains("true")
    )

    val orphanDirectoryNames: Set[String] =
      layout.collections.listFiles.filter(_.isDirectory).map(_.getName).toSet -- result.map(_.directoryName).toSet

    if (orphanDirectoryNames.nonEmpty)
      throw new IllegalArgumentException(s"Orphan directories: ${orphanDirectoryNames.mkString(", ")}")

    result
  }
}
