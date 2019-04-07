package org.podval.archive19kislev.collector

import java.io.File

object Main {
  private val docsDirectory: File = new File("docs").getAbsoluteFile

  private val namesFileName: String = "names"

  private val collections: Seq[Collection] = Seq(
    new Collection(docsDirectory, "archive", "Архив"),
    new Collection(docsDirectory, "dubnov", "Дубнов")
  )

  private val names: Names = new Names(docsDirectory, namesFileName)

  private val references: Seq[Name] =
    names.names.toSeq ++
    (for { collection <- collections; document <- collection.documents } yield document.names).flatten

  private def report(): Unit = {
    def section(name: String, references: Seq[Name]): Seq[String] =
    if (references.isEmpty) Seq.empty
    else s"## $name references ##" +: (for (reference <- references) yield s" - ${reference.display}")

    val missing: Seq[Name] = references.filter(_.isMissing).filterNot(_.name == "?")
    val malformed: Seq[Name] = references.filter(_.isMalformed)
    val unresolved: Seq[Name] = references.filter(_.isResolvable)
      .filter(reference => names.find(reference.ref.get).isEmpty)

    val failed: Boolean = missing.nonEmpty || malformed.nonEmpty || unresolved.nonEmpty

    val toStrings: Seq[String] =
      section("Missing", missing) ++
      section("Malformed", malformed) ++
      section("Unresolved", unresolved)

    Util.write(Main.docsDirectory, "status.md",
      Seq(
        "---",
        "title: Status report",
        "layout: page",
        "---"
      ) ++
        toStrings
    )
    if (failed)
      throw new IllegalArgumentException("\nTEI validation failed!\n" + toStrings.mkString("\n"))
  }

  def main(args: Array[String]): Unit = {
    println("Writing collection index files.")
    collections.foreach(_.writeIndex())

//    println("Writing wrappers.")
//    collections.foreach(_.writeWrappers())

    println("Adding back references.")
    names.addReferenced(references)

    println("Validating TEI files.")
    report()
  }
}
