package org.opentorah.fop.util

import java.io.File
import org.slf4j.{Logger, LoggerFactory}

object Files {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def writeInto(file: File, replace: Boolean, content: String): Unit =
    if (!replace && file.exists) {
      logger.debug(s"Already exists: $file")
    } else {
      logger.debug(s"Writing $file")
      org.opentorah.util.Files.write(file, content)
    }
}
