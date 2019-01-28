package org.podval.docbook.gradle

import org.gradle.api.logging.Logger
import java.io.{File, InputStream}

object ConfigurationInitializer {
  private val configurationResources: Seq[String] = Seq(
    "css/docbook.css",
    "fop/fop.xconf",
    "xsl/common.xsl",
    "xsl/common-html.xsl",
    "xsl/epub.xsl",
    "xsl/fo.xsl",
    "xsl/html.xsl"
  )

  def doIt(directory: File, logger: Logger): Unit = {
    def copyResource(name: String): Unit = {
      val is: InputStream = getClass.getResourceAsStream("/" + name)
      if (is == null) {
        logger.error(s"Configuration resource not found: $name")
      } else {
        val file = new File(directory, name)
        if (file.exists()) {
          logger.info(s"Skipping configuration resource $name: file $file already exists", null, null)
        } else {
          logger.info(s"Copying configuration resource $name to $file", null, null)
          file.getParentFile.mkdirs
          java.nio.file.Files.copy(is, file.toPath)
        }
      }
    }

    configurationResources.foreach(copyResource)
  }
}
