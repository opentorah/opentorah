package org.opentorah.docbook.plugin

import org.gradle.api.tasks.TaskAction
import org.gradle.api.{Action, DefaultTask, Plugin, Project}
import org.opentorah.docbook.Layout
import org.opentorah.fop.FopFonts
import org.opentorah.util.Gradle

final class DocBookPlugin extends Plugin[Project] {

  def apply(project: Project): Unit = {
//    project.logger.lifecycle(Util.applicationString)

    val extension: Extension = project.getExtensions.create("docBook", classOf[Extension], project)

    val processDocBookTask: ProcessDocBookTask = project.getTasks.create("processDocBook", classOf[ProcessDocBookTask])
    processDocBookTask.getXslt1version().set(extension.getXslt1version())
    processDocBookTask.getXslt2version().set(extension.getXslt2version())
    processDocBookTask.getDocument().set(extension.getDocument())
    processDocBookTask.getDocuments().set(extension.getDocuments())
    processDocBookTask.getParameters().set(extension.getParameters())
    processDocBookTask.getSubstitutions().set(extension.getSubstitutions())
    processDocBookTask.getCssFile().set(extension.getCssFile())
    processDocBookTask.getEpubEmbeddedFonts().set(extension.getEpubEmbeddedFonts())
    processDocBookTask.getOutputFormats().set(extension.getOutputFormats())
    processDocBookTask.getDataGeneratorClass().set(extension.getDataGeneratorClass())
    processDocBookTask.getJEuclidEnabled().set(extension.getJEuclidEnabled())
    processDocBookTask.getMathJaxEnabled().set(extension.mathJax.getEnabled())
    processDocBookTask.getUseMathJax3().set(extension.mathJax.getUseMathJax3())
    processDocBookTask.getNodeVersion().set(extension.mathJax.getNodeVersion())
    processDocBookTask.getUseJ2V8().set(extension.mathJax.getUseJ2V8())
    processDocBookTask.getMathJaxFont().set(extension.mathJax.getFont())
    processDocBookTask.getMathJaxExtensions().set(extension.mathJax.getExtensions())
    processDocBookTask.getTexDelimiter().set(extension.mathJax.getTexDelimiter())
    processDocBookTask.getTexInlineDelimiter().set(extension.mathJax.getTexInlineDelimiter())
    processDocBookTask.getAsciiMathDelimiter().set(extension.mathJax.getAsciiMathDelimiter())
    processDocBookTask.getProcessMathJaxEscapes().set(extension.mathJax.getProcessEscapes())

    project.getTasks.create("listFopFonts", classOf[DocBookPlugin.ListFopFontsTask])
    project.getTasks.create("deleteFopFontsCache", classOf[DocBookPlugin.DeleteFopFontsCacheTask])

    project.afterEvaluate(new Action[Project] {
      override def execute(project: Project): Unit = {
        val logger = project.getLogger
        def info(message: String): Unit = logger.info(message, null, null, null)

        // Note: even when DocBook plugin is applied after the Scala one,
        // there is no 'classes' task during its application - but there is after project evaluation:
        Gradle.getClassesTask(project).fold(info("No 'classes' task found.")){ classesTask =>
          info("Found 'classes' task; adding it as dependency of 'processDocBook'.")
          processDocBookTask.getDependsOn.add(classesTask)
        }
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
