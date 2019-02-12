package org.podval.docbook.gradle

import org.gradle.api.{Plugin, Project}
import scala.collection.JavaConverters._

// Properties are annotated with @BeanProperty to make them visible to Gradle.
final class DocBookPlugin extends Plugin[Project] {

  def apply(project: Project): Unit = {
    val layouts: Layouts = Layouts.forProject(project)

    // Configurations that resolves DocBook XSL stylesheets.
    createConfiguration(project, layouts.forXslt1.docBookXslConfigurationName,
      "net.sf.docbook", "docbook-xsl", "1.79.1", ":resources@zip")

    createConfiguration(project, layouts.forXslt2.docBookXslConfigurationName,
      "org.docbook", "docbook-xslt2", "2.3.10", "@jar")

    // Extension for configuring the plugin.
    val extension: Extension = project.getExtensions.create("docBook", classOf[Extension], project)
    extension.isJEuclidEnabled.set(false)
    extension.documentName.set("index")
    extension.dataGeneratorClass.set("")
    extension.outputFormats.set(DocBook2.forXslt1.map(_.name).asJava)
    extension.outputFormats2.set(List.empty[String].asJava)

    // Generate content that needs to be included in DocBook by executing the generating code.
    val dataTask: DocBookDataTask = project.getTasks.create("docBookData", classOf[DocBookDataTask])
    dataTask.setDescription("Generate data for inclusion in DocBook")
    dataTask.dataGeneratorClass.set(extension.dataGeneratorClass)
    Option(project.getTasks.findByName("classes")).foreach(dataTask.getDependsOn.add)

    // Process DocBook.
    val docBookTask: DocBookTask = project.getTasks.create("processDocBook", classOf[DocBookTask])
    docBookTask.setDescription(s"Process DocBook")
    docBookTask.setGroup("publishing")
    docBookTask.isJEuclidEnabled.set(extension.isJEuclidEnabled)
    docBookTask.inputFileName.set(extension.documentName)
    docBookTask.xslParameters.set(extension.xslParameters)
    docBookTask.substitutions.set(extension.substitutions)
    docBookTask.outputFormats.set(extension.outputFormats)
    docBookTask.outputFormats2.set(extension.outputFormats2)
    docBookTask.epubEmbeddedFonts.set(extension.epubEmbeddedFonts)
    docBookTask.getDependsOn.add(dataTask)

    // List fonts known to FOP.
    val listFontsTask: ListFopFontsTask = project.getTasks.create("listFopFonts", classOf[ListFopFontsTask])
    listFontsTask.setDescription("List FOP fonts")
  }

  private def createConfiguration(
    project: Project,
    name: String,
    groupId: String,
    artifactId: String,
    version: String,
    what: String
  ): Unit = project.getConfigurations.create(name).defaultDependencies(
    _.add(project.getDependencies.create(s"$groupId:$artifactId:$version$what")) : Unit
  ).setVisible(false)
}
