package org.podval.docbook.gradle

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction
import java.io.File

class ListFopFontsTask extends DefaultTask {
  @TaskAction
  def list(): Unit = {
    // FOP configuration file is the same for XSLT 1.0 and 2.0
    val configurationFile: File = Layouts.forProject(getProject).forXslt1.fopConfigurationFile
    Fop.listFonts(configurationFile)
  }
}
