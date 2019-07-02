package org.podval.docbook.gradle.plugin

import org.gradle.api.tasks.TaskAction
import org.gradle.api.{DefaultTask, Plugin, Project}
import org.podval.docbook.gradle.fop.Fop
import org.podval.docbook.gradle.util.Gradle

final class DocBookPlugin extends Plugin[Project] {

  def apply(project: Project): Unit = {
//    val logger = Logger.forProject(project)
//    logger.lifecycle(Util.applicationString)

    val extension: Extension = project.getExtensions.create("docBook", classOf[Extension], project)

    val processDocBookTask: ProcessDocBookTask = project.getTasks.create("processDocBook", classOf[ProcessDocBookTask])
    processDocBookTask.setDescription(s"Process DocBook")
    processDocBookTask.setGroup("publishing")
    processDocBookTask.xslt1version.set(extension.xslt1version)
    processDocBookTask.xslt2version.set(extension.xslt2version)
    processDocBookTask.document.set(extension.document)
    processDocBookTask.documents.set(extension.documents)
    processDocBookTask.parameters.set(extension.parameters)
    processDocBookTask.substitutions.set(extension.substitutions)
    processDocBookTask.cssFile.set(extension.cssFile)
    processDocBookTask.epubEmbeddedFonts.set(extension.epubEmbeddedFonts)
    processDocBookTask.outputFormats.set(extension.outputFormats)
    processDocBookTask.dataGeneratorClass.set(extension.dataGeneratorClass)
    processDocBookTask.isJEuclidEnabled.set(extension.isJEuclidEnabled)
    processDocBookTask.isMathJaxEnabled.set(extension.mathJax.isEnabled)
    processDocBookTask.useJ2V8.set(extension.mathJax.useJ2V8)
    processDocBookTask.mathJaxFont.set(extension.mathJax.font)
    processDocBookTask.mathJaxExtensions.set(extension.mathJax.extensions)
    processDocBookTask.texDelimiter.set(extension.mathJax.texDelimiter)
    processDocBookTask.texInlineDelimiter.set(extension.mathJax.texInlineDelimiter)
    processDocBookTask.asciiMathDelimiter.set(extension.mathJax.asciiMathDelimiter)
    processDocBookTask.processMathJaxEscapes.set(extension.mathJax.processEscapes)

    Gradle.getClassesTask(project).foreach(processDocBookTask.getDependsOn.add)

    project.getTasks.create("listFopFonts", classOf[DocBookPlugin.ListFopFontsTask])
    project.getTasks.create("deleteFopFontsCache", classOf[DocBookPlugin.DeleteFopFontsCacheTask])
  }
}

object DocBookPlugin {

  private class ListFopFontsTask extends DefaultTask {
    setDescription("List FOP fonts")

    @TaskAction def execute(): Unit =
      Fop.listFonts(Layout.forProject(getProject).fopConfigurationFile)
  }

  private class DeleteFopFontsCacheTask extends DefaultTask {
    setDescription("Delete FOP fonts cache")

    @TaskAction def execute(): Unit =
      Fop.deleteFontCache(Layout.forProject(getProject).fopConfigurationFile)
  }
}
