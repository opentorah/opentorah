package org.opentorah.fop.util

import java.io.File

object Files {

  def writeInto(file: File, replace: Boolean, content: String, logger: Logger): Unit =
    if (!replace && file.exists) {
      logger.debug(s"Already exists: $file")
    } else {
      logger.debug(s"Writing $file")
      org.opentorah.util.Files.write(file, content)
    }
}
