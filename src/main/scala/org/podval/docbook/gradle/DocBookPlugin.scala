package org.podval.docbook.gradle

import org.gradle.api.{Plugin, Project}
import scala.collection.JavaConverters._

final class DocBookPlugin extends Plugin[Project] {

  def apply(project: Project): Unit = {
    val logger: Logger = new Logger.PluginLogger(project.getLogger)
    val info = getClass.getPackage
    logger.lifecycle(info.getImplementationTitle + " v" + info.getImplementationVersion + ".")

    // Extension for configuring the plugin.
    val extension: Extension = project.getExtensions.create("docBook", classOf[Extension], project)
    extension.xslt1version.set("+")
    extension.xslt2version.set("+")
    extension.document.set("index")
    extension.dataGeneratorClass.set("")
    extension.outputFormats.set(DocBook2.processors.filterNot(_.usesDocBookXslt2) .map(_.name).asJava)
    extension.cssFile.set("docBook")
    extension.isJEuclidEnabled.set(false)
    extension.epubEmbeddedFonts.set(List.empty[String].asJava)

    // Generate data for inclusion in DocBook by executing the generating code.
    val docBookDataTask: DocBookDataTask = project.getTasks.create("docBookData", classOf[DocBookDataTask])
    docBookDataTask.setDescription("Generate data for inclusion in DocBook")
    docBookDataTask.dataGeneratorClass.set(extension.dataGeneratorClass)
    Option(project.getTasks.findByName("classes")).foreach(docBookDataTask.getDependsOn.add)

    // Prepare DocBook.
    val prepareDocBookTask: PrepareDocBookTask = project.getTasks.create("prepareDocBook", classOf[PrepareDocBookTask])
    prepareDocBookTask.setDescription(s"Prepare DocBook")
    prepareDocBookTask.xslt1version.set(extension.xslt1version)
    prepareDocBookTask.xslt2version.set(extension.xslt2version)
    prepareDocBookTask.document.set(extension.document)
    prepareDocBookTask.parameters.set(extension.parameters)
    prepareDocBookTask.substitutions.set(extension.substitutions)
    prepareDocBookTask.cssFile.set(extension.cssFile)
    prepareDocBookTask.epubEmbeddedFonts.set(extension.epubEmbeddedFonts)
    prepareDocBookTask.getDependsOn.add(docBookDataTask)

    // Process DocBook.
    val processDocBookTask: ProcessDocBookTask = project.getTasks.create("processDocBook", classOf[ProcessDocBookTask])
    processDocBookTask.setDescription(s"Process DocBook")
    processDocBookTask.setGroup("publishing")
    processDocBookTask.document.set(extension.document)
    processDocBookTask.parameters.set(extension.parameters)
    processDocBookTask.substitutions.set(extension.substitutions)
    processDocBookTask.outputFormats.set(extension.outputFormats)
    processDocBookTask.isJEuclidEnabled.set(extension.isJEuclidEnabled)
    processDocBookTask.getDependsOn.add(docBookDataTask)
    processDocBookTask.getDependsOn.add(prepareDocBookTask)

    // List fonts known to FOP.
    val listFontsTask: ListFopFontsTask = project.getTasks.create("listFopFonts", classOf[ListFopFontsTask])
    listFontsTask.setDescription("List FOP fonts")
  }
}
