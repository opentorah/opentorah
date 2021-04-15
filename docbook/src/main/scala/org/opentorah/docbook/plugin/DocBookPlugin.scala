package org.opentorah.docbook.plugin

import org.gradle.api.tasks.TaskAction
import org.gradle.api.{DefaultTask, Plugin, Project}
import org.opentorah.docbook.Layout
import org.opentorah.fop.FopFonts
import org.opentorah.util.Gradle

final class DocBookPlugin extends Plugin[Project] {

  def apply(project: Project): Unit = {
//    project.logger.lifecycle(Util.applicationString)

    val extension: Extension = project.getExtensions.create("docBook", classOf[Extension], project)

    val processDocBookTask: ProcessDocBookTask = project.getTasks.create("processDocBook", classOf[ProcessDocBookTask])
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
    processDocBookTask.useMathJax3.set(extension.mathJax.useMathJax3)
    processDocBookTask.nodeVersion.set(extension.mathJax.nodeVersion)
    processDocBookTask.useJ2V8.set(extension.mathJax.useJ2V8)
    processDocBookTask.mathJaxFont.set(extension.mathJax.font)
    processDocBookTask.mathJaxExtensions.set(extension.mathJax.extensions)
    processDocBookTask.texDelimiter.set(extension.mathJax.texDelimiter)
    processDocBookTask.texInlineDelimiter.set(extension.mathJax.texInlineDelimiter)
    processDocBookTask.asciiMathDelimiter.set(extension.mathJax.asciiMathDelimiter)
    processDocBookTask.processMathJaxEscapes.set(extension.mathJax.processEscapes)

    project.getTasks.create("listFopFonts", classOf[DocBookPlugin.ListFopFontsTask])
    project.getTasks.create("deleteFopFontsCache", classOf[DocBookPlugin.DeleteFopFontsCacheTask])

    project.afterEvaluate((project: Project) => {
      val logger = project.getLogger
      def info(message: String): Unit = logger.info(message, null, null, null)

      // Note: even when DocBook plugin is applied after the Scala one,
      // there is no 'classes' task during its application - but there is after project evaluation:
      Gradle.getClassesTask(project).fold(info("No 'classes' task found.")){ classesTask =>
        info("Found 'classes' task; adding it as dependency of 'processDocBook'.")
        processDocBookTask.getDependsOn.add(classesTask)
      }
    })
  }
}

object DocBookPlugin {

  /* not final so that Gradle could do its thing */ class ListFopFontsTask extends DefaultTask {
    setDescription("List FOP fonts")

    @TaskAction def execute(): Unit = {
      val result = FopFonts.list(configurationFile = layoutForProject(getProject).fopConfigurationFile)
      System.out.print(result)
      System.out.flush()
    }
  }

  /* not final so that Gradle could do its thing */ class DeleteFopFontsCacheTask extends DefaultTask {
    setDescription("Delete FOP fonts cache")

    @TaskAction def execute(): Unit =
      FopFonts.deleteCache(layoutForProject(getProject).fopConfigurationFile)
  }

  def layoutForProject(project: Project): Layout = new Layout(
    frameworksDir = project.getGradle.getGradleUserHomeDir,
    projectDir = project.getProjectDir,
    buildDir = project.getBuildDir,
    catalogDirectoryOverride = None,
    outputRootOverride = None
  )
}
