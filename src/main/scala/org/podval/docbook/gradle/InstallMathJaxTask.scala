package org.podval.docbook.gradle

import java.io.File

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction
import org.podval.docbook.gradle.node.Installation

class InstallMathJaxTask extends DefaultTask {

  private val layout: Layout = Layout.forProject(getProject)

  @TaskAction
  def installMathJax(): Unit = Installation.installMathJax(
    layout.nodeRoot,
    layout.nodeModulesRoot,
    getProject,
    new Logger.PluginLogger(getLogger)
  )
}
