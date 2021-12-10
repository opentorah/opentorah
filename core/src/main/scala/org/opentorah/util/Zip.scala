package org.opentorah.util

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.file.Files
import java.util.zip.{ZipEntry, ZipOutputStream}

private class Zip(output: File):
  private val buffer: Array[Byte] = new Array[Byte](Zip.bufferSize)
  private val zos: ZipOutputStream = ZipOutputStream(FileOutputStream(output))

  def add(content: Seq[File]): Unit =
    for file <- content do add(file, file.getName)
    zos.close()

  private def add(file: File, fileName: String): Unit =
    if file.isDirectory then
      val fileNameWithSlash = if fileName.endsWith("/") then fileName else fileName + "/"
      zos.putNextEntry(zipEntry(fileNameWithSlash, file))
      zos.closeEntry()

      for (child <- file.listFiles) add(child, fileNameWithSlash + child.getName)
    else
      zos.putNextEntry(zipEntry(fileName, file))
      writeFile(file)
      zos.closeEntry()

  private def zipEntry(name: String, file: File): ZipEntry =
    val result: ZipEntry = ZipEntry(name)
    result.setLastModifiedTime(Files.getLastModifiedTime(file.toPath))
    result

  private def writeFile(file: File): Unit =
    val fis: FileInputStream = FileInputStream(file)

    var done: Boolean = false
    while !done do
      val length: Int = fis.read(buffer)
      if length == 0 then done = true else zos.write(buffer, 0, length)

    fis.close()

object Zip:
  private val bufferSize: Int = 2048

  def zip(output: File, content: Seq[File]): Unit = Zip(output).add(content)
