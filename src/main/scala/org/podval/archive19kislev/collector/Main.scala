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

    println(s"Splicing ${Layout.indexMdFileName}.")
    Util.splice(
      file = Layout.docs(Layout.indexMdFileName),
      start = """<a name="collections-start">""",
      end = """<a name="collections-end">""",
      what = collections.flatMap { collection => Seq(
        s"""- <a href="/${collection.directoryName}" target="collectionViewer">${collection.reference}</a>:""",
        s"${collection.title}."
      )}
    )

    println(s"Splicing ${Layout.configYmlFileName}.")
    Util.splice(
      file = Layout.docs(Layout.configYmlFileName),
      start = "# collections-start",
      end = "# collections-end",
      what = collections.map { collection =>
        s"  - ${collection.directoryName}/index.html"
      }
    )

    println("Processing name references.")
    val report: Option[Seq[String]] = new Names().processReferences(documentReferences =
      (for { collection <- collections; document <- collection.documents } yield document.names).flatten)

    report.foreach(report =>
      throw new IllegalArgumentException("\nTEI validation failed!\n" + report.mkString("\n")))
  }

  // Collections listed in collections.xml in the order they are listed there -
  // or directories with collection.xml in alphabetical order.
  private def getCollections: Seq[Collection] = {
    def collectionXmlFile(directory: File): File = new File(directory, Layout.collectionXmlFileName)
    def isFile(file: File): Boolean = file.exists() && file.isFile

    val directoryNames: Seq[String] = {
      for {
        file <- Some(Layout.docs(Layout.collectionsXmlFileName))
        if isFile(file)
      } yield Xml.load(file)
        .check(name = "collections")
        .elems(name = "collection")
        .map(_.text)
    }.getOrElse {
      for {
        directory <- Layout.docsRoot.listFiles.toSeq.sorted
        if directory.isDirectory && isFile(collectionXmlFile(directory))
      } yield directory.getName
    }

    for (directoryName <- directoryNames) yield {
      val directory: File = Layout.docs(directoryName)
      val xml: Elem = Xml.load(collectionXmlFile(directory))
      new Collection(directory, xml)
    }
  }
}
