package org.podval.archive19kislev.collector

import java.io.File

object Main {
  val docsDirectory: File = new File("docs").getAbsoluteFile

  val documentsDirectoryName: String = "documents"
  val teiDirectoryName: String = "tei"
  val facsimilesDirectoryName: String = "facsimiles"

  val namesFileName: String = "names"

  val collections: Seq[Collection] = Seq(
    new Collection("dubnov"),
    new Collection("archive")
  )

  val names: Names = new Names(docsDirectory, namesFileName)

  def checkNames(collection: Collection) {
    println(s"Names for ${collection.directoryName}")
    for (document <- collection.documents) {
      val undefined = document.references.filter(_.ref.isEmpty).map(_.name)
      if (undefined.nonEmpty)
        println(s"Undefined references in $document: ${undefined.mkString(", ")}")
    }

    val references: Set[String] = collection.documents.flatMap { document =>
      document.references
        .filter(_.ref.isDefined)
        .map(_.ref.get)
        .toSet
    }.toSet

    val unresolved: Set[String] = references.filter(names.find(_).isEmpty)
    if (unresolved.nonEmpty) {
      println()
      println("Unresolved references:")
      unresolved.foreach(reference => println(s"  $reference"))
    }
  }

  def main(args: Array[String]): Unit = {
    collections.foreach { collection =>
      collection.writeIndex()
//      collection.writeWrappers()
    }
    collections.foreach(checkNames)
  }
}
