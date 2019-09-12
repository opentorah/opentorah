package org.podval.archive19kislev.collector

import java.io.File

import scala.xml.Elem
import Xml.Ops

object Main {

  def main(args: Array[String]): Unit = {
    val collections: Seq[Collection] = getCollections

    println("Collections:")
    println(collections.map { collection =>
      s"  ${collection.directoryName}: ${collection.title}\n"
    }.mkString)

    println("Processing collections.")
    collections.foreach(_.process())

    Util.splice(
      file = Layout.indexMd,
      start = """<a name="collections-start">""",
      end = """<a name="collections-end">""",
      what = collections.flatMap { collection => Seq(
        s"""- <a href="${Layout.collectionUrl(collection.directoryName)}" target="collectionViewer">${collection.reference}</a>:""",
        s"${collection.title}."
      )}
    )

    Util.splice(
      file = Layout.configYml,
      start = "# collections-start",
      end = "# collections-end",
      what = collections.map { collection =>
        val url: String = Layout.collectionUrl(collection.directoryName)
        // Links with starting slash do not make it into the navigation bar?
        val ref: String = if (url.startsWith("/")) url.substring(1) else url
        s"  - $ref"
      }
    )

    println("Processing name references.")
    val report: Option[Seq[String]] = new Names().processReferences(documentReferences = collections.flatMap(_.names))

    report.foreach(report =>
      throw new IllegalArgumentException("\nTEI validation failed!\n" + report.mkString("\n")))
  }

  // Collections listed in collections.xml in the order they are listed there -
  // or directories with collection.xml in alphabetical order.
  private def getCollections: Seq[Collection] = {
    def isFile(file: File): Boolean = file.exists() && file.isFile

    val collectionNames: Seq[String] = {
      for {
        file <- Some(Layout.collectionsXml)
        if isFile(file)
      } yield Xml.load(file)
        .check(name = "collections")
        .elems(name = "collection")
        .map(_.text)
    }.getOrElse {
      for {
        directory <- Layout.collections.listFiles.toSeq.sorted
        if directory.isDirectory && isFile(Layout.collectionXml(directory))
      } yield directory.getName
    }

    for (collectionName <- collectionNames) yield {
      val directory: File = Layout.collections(collectionName)
      val xml: Elem = Xml.load(Layout.collectionXml(directory))
      new Collection(directory, xml)
    }
  }
}
