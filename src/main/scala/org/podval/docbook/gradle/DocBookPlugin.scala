package org.podval.docbook.gradle

import org.gradle.api.{Plugin, Project}
import java.io.{File, InputStream}
import scala.collection.JavaConverters._

// Properties are annotated with @BeanProperty to make them visible to Gradle.
final class DocBookPlugin extends Plugin[Project] {

  def apply(project: Project): Unit = {
    val layout: Layout = new Layout(project)

    // Configurations that resolves DocBook XSL stylesheets.
    val docBookXslVersion: String = "1.79.1"
    project.getConfigurations.create(layout.docBookXslConfigurationName).defaultDependencies(
      _.add(project.getDependencies.create(s"net.sf.docbook:docbook-xsl:$docBookXslVersion:resources@zip")) : Unit
    ).setVisible(false)

    val docBookXsl2Version: String = "2.3.10"
    project.getConfigurations.create(layout.docBookXsl2ConfigurationName).defaultDependencies(
      _.add(project.getDependencies.create(s"org.docbook:docbook-xslt2:$docBookXsl2Version@jar")) : Unit
    ).setVisible(false)

    // Write default XSLT customizations, CSS stylesheet and FOP configuration.
    initializeProject(layout, new Logger.PluginLogger(project.getLogger))

    // Extension for configuring the plugin.
    val extension: DocBookExtension = project.getExtensions.create("docBook", classOf[DocBookExtension], project)
    extension.documentName.set("index")
    extension.dataGeneratorClass.set("")
    extension.outputFormats.set(DocBook2.availableFormats.asJava)

    // Generate content that needs to be included in DocBook by executing the generating code.
    val dataTask: PrepareDocBookDataTask = project.getTasks.create("docBookData", classOf[PrepareDocBookDataTask])
    dataTask.setDescription("Generate data for inclusion in DocBook")
    dataTask.dataGeneratorClass.set(extension.dataGeneratorClass)
    Option(project.getTasks.findByName("classes")).foreach(dataTask.getDependsOn.add)

    // Process DocBook.
    val docBookTask: DocBookTask = project.getTasks.create("processDocBook", classOf[DocBookTask])
    docBookTask.setDescription(s"Process DocBook into ${DocBook2.availableFormatNames}")
    docBookTask.setGroup("publishing")
    docBookTask.inputFileName.set(extension.documentName)
    docBookTask.xslParameters.set(extension.xslParameters)
    docBookTask.substitutions.set(extension.substitutions)
    docBookTask.outputFormats.set(extension.outputFormats)
    docBookTask.epubEmbeddedFonts.set(extension.epubEmbeddedFonts)
    docBookTask.getDependsOn.add(dataTask)

    // List fonts known to FOP.
    val listFontsTask: ListFopFontsTask = project.getTasks.create("listFopFonts", classOf[ListFopFontsTask])
    listFontsTask.setDescription("List FOP fonts")
  }

  private def initializeProject(layout: Layout, logger: Logger): Unit = {
    copyResource("css", "docBook.css", layout.cssFile, logger)
    copyResource("fop", "fop.xconf", layout.fopConfigurationFile, logger)

    Seq("common", "common-html", "epub", "fo", "html").foreach { name: String =>
      copyResource("xsl", name + ".xsl", layout.stylesheetFile(name, false), logger)
    }
  }

  private def copyResource(prefix: String, name: String, file: File, logger: Logger): Unit = {
    if (file.exists()) {
      logger.info(s"Skipping configuration resource $name: file $file already exists")
    } else {
      logger.info(s"Copying configuration resource $name to $file")
      file.getParentFile.mkdirs

      val is: InputStream = getClass.getResourceAsStream("/" + prefix + "/" + name)
      if (is == null) {
        val message: String = s"Configuration resource not found: $name"
        logger.error(message)
        throw new IllegalArgumentException(message)
      }
      java.nio.file.Files.copy(is, file.toPath)
    }
  }
}
