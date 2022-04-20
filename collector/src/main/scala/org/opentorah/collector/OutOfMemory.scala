package org.opentorah.collector

import org.opentorah.store.{ListFile, WithSource}
import org.opentorah.tei.EntityReference
import org.opentorah.util.Files
import org.opentorah.xml.{Caching, Parser}
import java.io.File

object OutOfMemory:
  @main def run(): Unit =
    val file: File = File("/home/dub/OpenTorah/alter-rebbe.org/references-generated.xml")
    if file.exists then
      val listFile: ListFile[WithSource[EntityReference], Seq[WithSource[EntityReference]]] = WithSource(
        url = Files.file2url(file),
        name = "references",
        value = EntityReference
      )
      val references: Seq[WithSource[EntityReference]] = Parser.unsafeRun(
        Caching.provide(new Caching.Simple, listFile.get)
      )
      println(s"# references: ${references.length}")
