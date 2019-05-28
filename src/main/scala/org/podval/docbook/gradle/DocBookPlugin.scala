package org.podval.docbook.gradle

import org.gradle.api.{Plugin, Project}
import org.podval.docbook.gradle.section.DocBook2
import org.podval.docbook.gradle.util.Util

import scala.collection.JavaConverters._

final class DocBookPlugin extends Plugin[Project] {

  def apply(project: Project): Unit = {
    new Logger.PluginLogger(project.getLogger).lifecycle(Util.applicationString)

    // j2v8 bindings
    // doesn't work: project.getBuildscript.getDependencies.add(ScriptHandler.CLASSPATH_CONFIGURATION, MathJax.j2v8dependency)

    // Extension for configuring the plugin.
    val extension: Extension = project.getExtensions.create("docBook", classOf[Extension], project)
    extension.xslt1version.set("+")
    extension.xslt2version.set("+")
    extension.document.set("")
    extension.documents.set(List.empty.asJava)
    extension.dataGeneratorClass.set("")
    extension.outputFormats.set(DocBook2.all.filterNot(_.usesDocBookXslt2) .map(_.name).asJava)
    extension.cssFile.set("docBook")
    extension.isMathJaxEnabled.set(false)
    extension.isJEuclidEnabled.set(false)
    extension.epubEmbeddedFonts.set(List.empty[String].asJava)

    // Generate data for inclusion in DocBook by executing the generating code.
    val docBookDataTask: DocBookDataTask = project.getTasks.create("docBookData", classOf[DocBookDataTask])
    docBookDataTask.setDescription("Generate data for inclusion in DocBook")
    docBookDataTask.dataGeneratorClass.set(extension.dataGeneratorClass)
    Option(project.getTasks.findByName("classes")).foreach(docBookDataTask.getDependsOn.add)

    // MathJax
    // TODO submerge into 'prepare' task (and remove as a dependency of the 'process' task)?
    val installMathJaxTask: InstallMathJaxTask = project.getTasks.create("installMathJax", classOf[InstallMathJaxTask])

    // Prepare DocBook.
    val prepareDocBookTask: PrepareDocBookTask = project.getTasks.create("prepareDocBook", classOf[PrepareDocBookTask])
    prepareDocBookTask.setDescription(s"Prepare DocBook")
    prepareDocBookTask.xslt1version.set(extension.xslt1version)
    prepareDocBookTask.xslt2version.set(extension.xslt2version)
    prepareDocBookTask.document.set(extension.document)
    prepareDocBookTask.documents.set(extension.documents)
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
    processDocBookTask.documents.set(extension.documents)
    processDocBookTask.parameters.set(extension.parameters)
    processDocBookTask.substitutions.set(extension.substitutions)
    processDocBookTask.outputFormats.set(extension.outputFormats)
    processDocBookTask.isMathJaxEnabled.set(extension.isMathJaxEnabled)
    processDocBookTask.isJEuclidEnabled.set(extension.isJEuclidEnabled)
    processDocBookTask.getDependsOn.add(docBookDataTask)
    processDocBookTask.getDependsOn.add(prepareDocBookTask)
    processDocBookTask.getDependsOn.add(installMathJaxTask)

    // List fonts known to FOP.
    val listFontsTask: ListFopFontsTask = project.getTasks.create("listFopFonts", classOf[ListFopFontsTask])
    listFontsTask.setDescription("List FOP fonts")
  }
}
