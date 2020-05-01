package org.opentorah.docbook.plugin

import org.gradle.api.tasks.TaskAction
import org.gradle.api.{DefaultTask, Plugin, Project}
import org.opentorah.fop.FopFonts

final class DocBookPlugin extends Plugin[Project] {

  def apply(project: Project): Unit = {
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

    project.getTasks.create("listFopFonts", classOf[DocBookPlugin.ListFopFontsTask])
    project.getTasks.create("deleteFopFontsCache", classOf[DocBookPlugin.DeleteFopFontsCacheTask])

// TODO update plugin instructions.
// Data generation class doesn't have to reside in the same project where DocBook plugin is configured,
// so not adding dependency on 'classes';
// also, to include processDocBook in the 'build' or not - is a policy decision best left to the user
// of the plugin...
//    project.afterEvaluate((project: Project) => {
//      val logger: Logger = PluginLogger.forProject(project)
//
//      // Note: even when DocBook plugin is applied after the Scala one,
//      // there is no 'classes' task during its application - but there is after project evaluation:
//      Gradle.getTask(project, "classes").fold {
//        logger.info("No 'classes' task found.")
//      }{ classesTask =>
//        logger.info("Found 'classes' task; adding it as dependency of 'processDocBook'.")
//        processDocBookTask.getDependsOn.add(classesTask)
//      }
//
//      Gradle.getTask(project, "build").fold {
//        logger.info("No 'build' task found.")
//      }{ buildTask =>
//        logger.info("Found 'build' task; adding 'processDocBook' as its dependency.")
//        buildTask.getDependsOn.add(processDocBookTask)
//      }
//    })
  }
}

object DocBookPlugin {

  private class ListFopFontsTask extends DefaultTask {
    setDescription("List FOP fonts")

    @TaskAction def execute(): Unit = {
      val result = FopFonts.list(configurationFile = Layout.forProject(getProject).fopConfigurationFile)
      // TODO logger.lifecycle()?
      System.out.print(result)
      System.out.flush()
    }
  }

  private class DeleteFopFontsCacheTask extends DefaultTask {
    setDescription("Delete FOP fonts cache")

    @TaskAction def execute(): Unit =
      FopFonts.deleteCache(Layout.forProject(getProject).fopConfigurationFile)
  }
}
