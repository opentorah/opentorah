package org.opentorah.files

import org.opentorah.util.Files
import java.io.File

// This is a replacement for Gradle-supplied Ant functionality for copying a bunch of files
// with or without substitutions. I need this since I want DocBook functionality to be usable
// outside of Gradle.
// Also, black-boxiness of the Ant stuff with its `"tokens" => ...` was just too much; good riddance!
private class Copy(substitutions: Map[String, String]):

  def copy(from: File, into: File): Unit =
    if from.isFile
    then copyFile(from, into)
    else copyChildren(from, File(into, from.getName))

  private def copyChildren(parent: File, into: File): Unit =
    into.mkdirs()
    for (child <- parent.listFiles) copy(child, into)

  private def copyFile(from: File, into: File): Unit =
    val to: File = File(into, from.getName)
    if substitutions.isEmpty then java.nio.file.Files.copy(
      from.toPath,
      to.toPath,
      java.nio.file.StandardCopyOption.REPLACE_EXISTING
    ) else
      val content: String = Files.read(from).mkString("", "\n", "\n")
      val newContent: String = substitutions.foldRight(content) {
        case ((name: String, value: String), content: String) => content.replaceAll(s"@$name@", value)
      }
      Files.write(file = to, newContent)

object Copy:
  def copyInto(from: File, into: File, substitutions: Map[String, String]): Unit =
    if from.exists then new Copy(substitutions).copy(from, into)

  def copyTo(from: File, into: File, substitutions: Map[String, String]): Unit =
    if from.exists then new Copy(substitutions).copyChildren(from, into)
