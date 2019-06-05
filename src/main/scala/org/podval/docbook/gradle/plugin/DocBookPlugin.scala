package org.podval.docbook.gradle.plugin

import org.gradle.api.{Plugin, Project}
import org.podval.docbook.gradle.section.DocBook2
import org.podval.docbook.gradle.util.{Gradle, Logger, Util}

import scala.jdk.CollectionConverters._

final class DocBookPlugin extends Plugin[Project] {

  def apply(project: Project): Unit = {
    val logger = Logger.forProject(project)
    logger.lifecycle(Util.applicationString)

    // j2v8 bindings
    // TODO doesn't work: project.getBuildscript.getDependencies.add(ScriptHandler.CLASSPATH_CONFIGURATION, MathJax.j2v8dependency)

    // Extension for configuring the plugin.
    val extension: Extension = project.getExtensions.create("docBook", classOf[Extension], project)
    extension.xslt1version.set("+")
    extension.xslt2version.set("+")
    extension.document.set("")
    extension.documents.set(List.empty.asJava)
    extension.dataGeneratorClass.set("")
    extension.outputFormats.set(DocBook2.all.filterNot(_.usesDocBookXslt2).map(_.name).asJava)
    extension.cssFile.set("docBook")
    extension.isMathJaxEnabled.set(false)
    extension.isJEuclidEnabled.set(false)
    extension.epubEmbeddedFonts.set(List.empty[String].asJava)

    // Process DocBook.
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
    processDocBookTask.isMathJaxEnabled.set(extension.isMathJaxEnabled)
    processDocBookTask.isJEuclidEnabled.set(extension.isJEuclidEnabled)
    processDocBookTask.dataGeneratorClass.set(extension.dataGeneratorClass)
    Gradle.getClassesTask(project).foreach(processDocBookTask.getDependsOn.add)

    // List fonts known to FOP.
    val listFontsTask: ListFopFontsTask = project.getTasks.create("listFopFonts",
      classOf[ListFopFontsTask])
    listFontsTask.setDescription("List FOP fonts")

    // Delete FOP fonts cache.
    val deleteFopFontsCacheTask: DeleteFopFontsCacheTask = project.getTasks.create("deleteFopFontsCache",
      classOf[DeleteFopFontsCacheTask])
    deleteFopFontsCacheTask.setDescription("Delete FOP fonts cache")
  }
}
