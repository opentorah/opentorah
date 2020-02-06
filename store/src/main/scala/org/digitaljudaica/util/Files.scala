package org.digitaljudaica.util

import java.io.File

object Files {
  def filesWithExtensions(directory: File, extension: String): Seq[String] = {
    (if (!directory.exists) Seq.empty else directory.listFiles.toSeq)
      .map(_.getName)
      .filter(_.endsWith(extension)).map(_.dropRight(extension.length))
  }
}
