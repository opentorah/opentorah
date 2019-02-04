package org.podval.docbook.gradle

import org.gradle.api.{Plugin, Project}
import org.gradle.api.tasks.Copy
import java.io.File
import scala.collection.JavaConverters._

import org.gradle.api.artifacts.Configuration

// Properties are annotated with @BeanProperty to make them visible to Gradle.
final class DocBookPlugin extends Plugin[Project] {

  def apply(project: Project): Unit = {
    def file(directory: File, name: String, extension: String): File = new File(directory, name + "." + extension)

    // Should get the main sourceSet, but this is only available via JavaConventions...
    // We *are* using it already in the PrepareDocBookDataTask though...
    val sourceRootDirectory: File = new File(project.getProjectDir, "src/main")
    def sourceDirectory(name: String): File = new File(sourceRootDirectory, name)
    def sourceFile(directory: String, name: String, extension: String): File =
      file(sourceDirectory(directory), name, extension)
    val fopConfigurationFile: File = sourceFile("fop", "fop", "xconf")

    val buildRootDirectory: File = project.getBuildDir
    def buildDirectory(name: String): File = new File(buildRootDirectory, name)

    val dataDirectory: File = buildDirectory("data")

    val explodeDocBookXslInto: File = buildDirectory("docBookXsl")

    ConfigurationInitializer.doIt(sourceRootDirectory, new Logger.PluginLogger(project.getLogger))

    val extension: DocBookExtension = project.getExtensions.create("docBook", classOf[DocBookExtension], project)
    extension.documentName.set("index")
    extension.dataGeneratorClass.set("")
    extension.outputFormats.set(DocBook2.outputFormats.asJava)

    // Download and unpack DocBook XSLT stylesheets
    val docBookXslConfiguration: Configuration = project.getConfigurations.create("docBookXsl").defaultDependencies(
      _.add(project.getDependencies.create("net.sf.docbook:docbook-xsl:1.79.1:resources@zip")) : Unit
    ).setVisible(false)

    val prepareTask: Copy = project.getTasks.create("docBookPrepare", classOf[Copy])
    prepareTask.setDescription("Prepare DocBook XSLT stylesheets")
    prepareTask.from(project.zipTree(docBookXslConfiguration.getSingleFile))
    prepareTask.into(explodeDocBookXslInto)

    // List fonts known to FOP.
    val listFontsTask: ListFontsTask = project.getTasks.create("listFopFonts", classOf[ListFontsTask])
    listFontsTask.setDescription("List fonts known to FOP")
    listFontsTask.configurationFile.set(fopConfigurationFile)

    // Generate content that needs to be included in DocBook by executing the generating code.
    val dataTask: PrepareDocBookDataTask = project.getTasks.create("docBookData", classOf[PrepareDocBookDataTask])
    dataTask.setDescription("Generate data for inclusion in DocBook")
    dataTask.dataDirectory.set(dataDirectory)
    dataTask.dataGeneratorClass.set(extension.dataGeneratorClass)
    Option(project.getTasks.findByName("classes")).foreach(dataTask.getDependsOn.add)

    val docBookTask: DocBookTask = project.getTasks.create("docBook", classOf[DocBookTask])
    docBookTask.setDescription("Process DocBook into HTML, PDF and EPUB")
    docBookTask.setGroup("publishing")
    docBookTask.inputFileName.set(extension.documentName)
    docBookTask.xslParameters.set(extension.xslParameters)
    docBookTask.entities.set(extension.entities)
    docBookTask.substitutions.set(extension.substitutions)
    docBookTask.outputFormats.set(extension.outputFormats)
    docBookTask.getDependsOn.add(prepareTask)
    docBookTask.getDependsOn.add(dataTask)
  }
}
