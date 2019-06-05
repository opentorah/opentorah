package org.podval.docbook.gradle.plugin

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction
import org.podval.docbook.gradle.fop.Fop

class DeleteFopFontsCacheTask extends DefaultTask {

  @TaskAction
  def listFopFonts(): Unit =
    Fop.deleteFontCache(Layout.forProject(getProject).fopConfigurationFile)
}
