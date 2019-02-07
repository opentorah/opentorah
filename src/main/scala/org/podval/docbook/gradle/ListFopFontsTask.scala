package org.podval.docbook.gradle

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction
import java.io.File

class ListFopFontsTask extends DefaultTask {
  @TaskAction
  def list(): Unit = {
    val configurationFile: File = new Layout(getProject).fopConfigurationFile
    Fop.listFonts(configurationFile)
  }
}
