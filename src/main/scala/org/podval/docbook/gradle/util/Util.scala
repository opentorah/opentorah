package org.podval.docbook.gradle.util

import java.io.File

object Util {
  private def fileNameAndExtension(nameWithExtension: String): (String, Option[String]) = {
    val lastDot: Int = nameWithExtension.lastIndexOf(".")
    if (lastDot == -1) (nameWithExtension, None)
    else (
      nameWithExtension.substring(0, lastDot),
      Some(nameWithExtension.substring(lastDot+1))
    )
  }

  def dropAllowedExtension(nameWihtExtension: String, allowedExtension: String): String = {
    val (name: String, extension: Option[String]) = fileNameAndExtension(nameWihtExtension)
    if (extension.nonEmpty && !extension.contains("xml"))
      throw new IllegalArgumentException(s"Extension must be '$allowedExtension' if present: $nameWihtExtension")
    name
  }

  def applicationString: String = {
    val info = getClass.getPackage
    info.getImplementationTitle + " version " + info.getImplementationVersion
  }

  def deleteRecursively(file: File): Unit = {
    if (file.isDirectory)
      file.listFiles.foreach(deleteRecursively)
    if (file.exists && !file.delete)
      throw new Exception(s"Unable to delete ${file.getAbsolutePath}")
  }

  def readFrom(file: File): String = {
    val source = scala.io.Source.fromFile(file)
    val result = source.getLines.mkString("\n")
    source.close()
    result
  }

  def prefixedDirectory(directory: File, prefix: Option[String]): File =
    prefix.fold(directory)(prefix => new File(directory, prefix))
}
