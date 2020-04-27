package org.podval.fop.util

import java.io.{BufferedWriter, File, FileWriter}

object Files {

  // TODO move into util.Files - once the logger moves?
  def writeInto(file: File, replace: Boolean, content: String, logger: Logger): Unit = {
    if (!replace && file.exists) {
      logger.debug(s"Already exists: $file")
    } else {
      logger.debug(s"Writing $file")
      file.getParentFile.mkdirs()
      val writer: BufferedWriter = new BufferedWriter(new FileWriter(file))

      try {
        writer.write(content.stripMargin)
      } finally {
        writer.close()
      }
    }
  }
}
