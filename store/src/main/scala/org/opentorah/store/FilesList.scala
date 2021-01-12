package org.opentorah.store

import java.net.URL
import org.opentorah.util.Files

object FilesList {

  def get(
    baseUrl: URL,
    directoryName: String,
    extension: String
  ): (Seq[String], String => URL) = {
    val directory: URL = Files.subdirectory(baseUrl, directoryName)
    (
      Files.filesWithExtensions(Files.url2file(directory), extension).sorted,
      (fileName: String) => Files.fileInDirectory(directory, fileName + "." + extension)
    )
  }
}
