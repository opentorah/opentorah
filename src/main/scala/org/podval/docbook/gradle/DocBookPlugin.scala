package org.podval.docbook.gradle

import org.gradle.api.{DefaultTask, Plugin, Project}
import org.gradle.api.logging.Logger
import org.gradle.api.provider.Provider
import org.gradle.api.tasks.Copy
import java.io.{File, InputStream}

// Properties are annotated with @BeanProperty to make them visible to Gradle.
final class DocBookPlugin extends Plugin[Project] {
  def apply(project: Project): Unit = {
    def file(directory: File, name: String, extension: String): File = new File(directory, name + "." + extension)

    // Should get the main sourceSet, but this is only available via JavaConventions...
    // We *are* using it already in the PrepareDocBookDataTask though...
    val srcMain: File = new File(project.getProjectDir, "src/main")

    DocBookPlugin.initConfig(srcMain, project.getLogger)

    def projectDirectory(name: String): File = new File(srcMain, name)
    def buildDirectory(name: String): File = new File(project.getBuildDir, name)

    val explodeDocBookXslInto: File = buildDirectory("docBookXsl")
    val docBookXsl: File = new File(explodeDocBookXslInto, "docbook")
    def xslFile(project: Project, name: String): File = file(projectDirectory("xsl"), name, "xsl")
    val imagesDirectory: File = projectDirectory("images")
    val cssDirectory: File = projectDirectory("css")
    val dataDirectory: File = buildDirectory("data")
    def outputFile(outputType: String)(name: String): File = file(buildDirectory(outputType), name, outputType)

    val extension: DocBookExtension = project.getExtensions.create("docbook", classOf[DocBookExtension], project)
    extension.inputFileName.set("index")
    extension.dataGeneratorClass.set("")

    val docBookXslConfiguration = project.getConfigurations.create("docbookxsl").defaultDependencies(
      _.add(project.getDependencies.create("net.sf.docbook:docbook-xsl:1.79.1:resources@zip")) : Unit
    ).setVisible(false)

    val inputFile: Provider[File] = extension.inputFileName.map(file(projectDirectory("docbook"), _, "xml"))

    val prepareTask: Copy = project.getTasks.create("docBookPrepare", classOf[Copy])
    prepareTask.setDescription("Prepare DocBook XSLT stylesheets")
    prepareTask.from(project.zipTree(docBookXslConfiguration.getSingleFile))
    prepareTask.into(explodeDocBookXslInto)

    // Generated content that needs to be included in DocBook is produced by executing the generating code.
    val dataTask: PrepareDocBookDataTask =
      project.getTasks.create("docBookData", classOf[PrepareDocBookDataTask])
    dataTask.setDescription("Generate data for inclusion in DocBook")
    dataTask.dataDirectory.set(dataDirectory)
    dataTask.dataGeneratorClass.set(extension.dataGeneratorClass)
    val classesTask = project.getTasks.findByName("classes")
    if (classesTask != null) {
      dataTask.getDependsOn.add(classesTask)
    }
    dataTask.getDependsOn.add(prepareTask)

    def setUpSaxonTask(task: SaxonTask, outputType: String): Unit = {
      task.inputFile.set(inputFile)
      task.dataDirectory.set(dataDirectory)
      task.imagesDirectory.set(imagesDirectory)
      task.xslParameters.set(extension.xslParameters)
      task.xslDirectory.set(docBookXsl)
      task.stylesheetFile.set(xslFile(project, outputType))
    }

    val htmlXsltTask: SaxonTask = SaxonTask(project, "docBookHtmlXslt")
    setUpSaxonTask(htmlXsltTask, "html")
    htmlXsltTask.setDescription("DocBook -> HTML")
    htmlXsltTask.outputFile.set(outputFile("html")("index"))
    htmlXsltTask.getDependsOn.add(dataTask)

    val htmlImagesTask: Copy = project.getTasks.create("docBookHtmlImages", classOf[Copy])
    htmlImagesTask.setDescription("Copy images into the HTML generated from DocBook")
    htmlImagesTask.from(imagesDirectory)
    htmlImagesTask.into(new File(buildDirectory("html"), "images"))
//    docBookHtmlImagesTask.getDependsOn.add(docBookHtmlXsltTask)

    // copy CSS files replacing substitution tokens with their values
    val htmlCssTask: FilteringCopyTask = project.getTasks.create("docBookHtmlCss", classOf[FilteringCopyTask])
    htmlCssTask.setDescription("Copy CSS into the HTML generated from DocBook")
    htmlCssTask.from(cssDirectory)
    htmlCssTask.into(new File(buildDirectory("html"), "css"))
    htmlCssTask.tokens.set(extension.xslParameters)
//    docBookHtmlCssTask.getDependsOn.add(docBookHtmlXsltTask)

    val htmlTask: DefaultTask = project.getTasks.create("docBookHtml", classOf[DefaultTask])
    htmlTask.setGroup("publishing")
    htmlTask.getDependsOn.add(htmlXsltTask)
    htmlTask.getDependsOn.add(htmlImagesTask)

    val foTask: SaxonTask = SaxonTask(project, "docBookFoXslt")
    setUpSaxonTask(foTask, "fo")
    foTask.setDescription("DocBook -> XSL-FO")
    foTask.outputFile.set(extension.inputFileName.map[File](outputFile("fo")))
    foTask.getDependsOn.add(dataTask)

    val pdfTask: FopTask = FopTask(project, name = "docBookPdf")
    pdfTask.setDescription("XSL-FO -> PDF")
    pdfTask.setGroup("publishing")
    pdfTask.inputFile.set(foTask.outputFile)
    pdfTask.configurationFile.set(projectDirectory("fop/fop.xconf"))
    pdfTask.outputFile.set(extension.inputFileName.map[File](outputFile("pdf")))
    pdfTask.getDependsOn.add(foTask)

    val processTask: DefaultTask = project.getTasks.create("docBookProcess", classOf[DefaultTask])
    processTask.setDescription("Process DocBook into HTML and PDF")
    processTask.setGroup("publishing")
    processTask.getDependsOn.add(htmlTask)
    processTask.getDependsOn.add(pdfTask)
  }
}

object DocBookPlugin {
  private val configurationResources: Seq[String] = Seq(
    "fop/fop.xconf",
    "xsl/common.xsl",
    "xsl/common-html.xsl",
    "xsl/epub.xsl",
    "xsl/fo.xsl",
    "xsl/html.xsl"
  )

  private def initConfig(directory: File, logger: Logger): Unit = {
    def copyResource(name: String): Unit = {
      val is: InputStream = getClass.getResourceAsStream("/" + name)
      if (is == null) {
        logger.error(s"Configuration resource not found: $name")
      } else {
        val file = new File(directory, name)
        if (file.exists()) {
          logger.info(s"Skipping configuration resource $name: file $file already exists", null, null)
        } else {
          logger.info(s"Copying configuration resource $name to $file", null, null)
          file.getParentFile.mkdirs
          java.nio.file.Files.copy(is, file.toPath)
        }
      }
    }

    configurationResources.foreach(copyResource)
  }
}
