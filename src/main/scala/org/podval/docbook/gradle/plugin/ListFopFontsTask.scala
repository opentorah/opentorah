package org.podval.docbook.gradle.plugin

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction
import org.podval.docbook.gradle.fop.Fop

class ListFopFontsTask extends DefaultTask {

  @TaskAction
  def listFopFonts(): Unit =
    Fop.listFonts(Layout.forProject(getProject).fopConfigurationFile)
}
