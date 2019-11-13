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

    val collections: Seq[Collection] = getCollections(layout, errors)

    println("Collections:")
    println(collections.map { collection =>
      s"  ${collection.directoryName}: ${collection.title} [${collection.includeInNavigation}]\n"
    }.mkString)

    errors.check()

    println("Processing collections.")
    collections.foreach(_.process())

    errors.check()

    val collectionLinks: Seq[String] = collections.flatMap { collection => Seq(
      s"""- <a href="${layout.collectionUrl(collection.directoryName)}" target="collectionViewer">${collection.reference}</a>:""",
      s"${collection.title}."
    )}

    Util.writeYaml(layout.collectionsMd, "page", Seq(
      "title" -> "Дела", "target" -> "collectionViewer"
    ), collectionLinks)

//    Util.splice(
//      file = layout.indexMd,
//      start = """<a name="collections-start">""",
//      end = """<a name="collections-end">""",
//      what = collectionLinks
//    )

    val navigationRefs: Seq[String] =
      (for (collection <- collections.filter(_.includeInNavigation))
       yield layout.collectionUrl(collection.directoryName)) :+
        layout.namesUrl

    Util.splice(
      file = layout.configYml,
      start = "# header_pages start",
      end = "# header_pages end",
      what = navigationRefs.map { url =>
        // Links with starting slash do not make it into the navigation bar?
        val ref = if (url.startsWith("/")) url.substring(1) else url
        s"  - $ref"
      }
    )

    println("Processing name references.")
    val names: Names = new Names(layout, errors)
    names.processReferences(collections.flatMap(_.references))

    errors.check()
  }

  // Collections listed in collections.xml in the order they are listed there -
  // or directories with collection.xml in alphabetical order.
  private def getCollections(layout: Layout, errors: Errors): Seq[Collection] = {
    val result: Seq[Collection] = for {
      xml: Elem <- Xml
        .load(layout.collections, layout.collectionsFileName)
        .check(name = "collections")
        .elems(name = "collection")
      name: String = xml.getAttribute("name")
      directory: File = layout.collections(name)
    } yield new Collection(
      layout,
      directory,
      Xml.load(directory, layout.collectionFileName),
      includeInNavigation = xml.attributeOption("includeInNavigation").contains("true"),
      isBook = xml.attributeOption("isBook").contains("true"),
      errors
    )

    for (orphanDirectoryName <-
           layout.collections.listFiles.filter(_.isDirectory).map(_.getName).toSet -- result.map(_.directoryName).toSet)
      errors.error(s"Orphan collection directory: $orphanDirectoryName")

    result
  }
}
