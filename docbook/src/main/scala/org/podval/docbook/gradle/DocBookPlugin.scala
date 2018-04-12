
package org.podval.docbook.gradle

import java.io.File

//import org.apache.tools.ant.filters.ReplaceTokens
//import org.gradle.api.file.FileTree
import org.gradle.api.tasks.Copy
import org.gradle.api.{DefaultTask, Plugin, Project}

//import scala.collection.JavaConverters._

final class DocBookPlugin extends Plugin[Project] {
  def apply(project: Project): Unit = {
    import DocBookPlugin._

    val extension: DocBookExtension = project.getExtensions.create("docbook", classOf[DocBookExtension], project)

    // TODO make version overridable in the project using this extension in docbook{ xslVersion = ...}
    //   and avoid "can't change after... resolved" - like 'zinc' for scala-plugin.

    val docBookXslConfiguration = project.getConfigurations.create("docbookxsl").defaultDependencies(
      _.add(project.getDependencies.create("net.sf.docbook:docbook-xsl:1.79.1:resources@zip")) : Unit
    ).setVisible(false)

    // TODO add highlighting using 'net.sf.xslthl:xslthl:2.1.0'

    val prepareDocBookTask: Copy = project.getTasks.create("prepareDocBook", classOf[Copy], (copy: Copy) => {
      copy.setDescription("Prepare DocBook XSLT stylesheets")
      copy.from(project.zipTree(docBookXslConfiguration.getSingleFile))
      copy.into(explodeDocBookXslInto(project))
//      copy.filter(Map("tokens" -> tokens.asJava).asJava, classOf[ReplaceTokens])
      ()
    })

    val prepareDocBookDataTask: DefaultTask = project.getTasks.create("prepareDocBookData", classOf[DefaultTask])
    prepareDocBookDataTask.setDescription("Generate data for inclusion in DocBook; placeholder task: attach via doLast{}")
    prepareDocBookDataTask.getDependsOn.add(prepareDocBookTask)

    val docBookHtmlTask: SaxonTask = SaxonTask(project,
      name = "docBookHtml",
      description = "DocBook -> HTML",
      inputFileName = extension.getInputFileName,
      stylesheetName = "html",
      dataDirectory = extension.getDataDirectory,
      outputFileNameOverride = Some("index"),
      outputType = "html"
    )
    docBookHtmlTask.getDependsOn.add(prepareDocBookDataTask)

    // TODO copy the CSS - with filtering.
    // copy { from "src/main/css"    into "$outputDirectory/css"    }

    val docBookFoTask: SaxonTask = SaxonTask(project,
      name = "docBookFo",
      description = "DocBook -> XSL-FO",
      inputFileName = extension.getInputFileName,
      stylesheetName = "pdf",
      dataDirectory = extension.getDataDirectory,
      outputType = "fo"
    )
    docBookFoTask.getDependsOn.add(prepareDocBookDataTask)

    val docBookPdfTask: FopTask = FopTask(project,
      name = "docBookPdf",
      description = "XSL-FO -> PDF",
      inputFile = docBookFoTask.outputFile,
      outputFileName = extension.getInputFileName
    )
    docBookPdfTask.getDependsOn.add(docBookFoTask)

    val processDocBookTask: DefaultTask = project.getTasks.create("processDocBook", classOf[DefaultTask])
    processDocBookTask.setDescription("Process DocBook into HTML and PDF")
    processDocBookTask.getDependsOn.add(docBookPdfTask)
    processDocBookTask.getDependsOn.add(docBookHtmlTask)
  }
}

object DocBookPlugin {
  def explodeDocBookXslInto(project: Project): File = buildDirectory(project, "docBookXsl")
  def docBookXsl(project: Project): File = new File(explodeDocBookXslInto(project), "docbook")

  def xslDir(project: Project): File = projectDirectory(project, "xsl")
  def xslFile(project: Project, name: String): File = file(xslDir(project), name, "xsl")

  def docBookDir(project: Project): File = projectDirectory(project, "docbook")

  def fopConfiguration(project: Project): File = projectDirectory(project, "fop/fop.xconf")

  def outputDirectory(project: Project, name: String): File = buildDirectory(project, name)

  def projectDirectory(project: Project, name: String): File = new File(srcMain(project), name)
  def buildDirectory(project: Project, name: String): File = new File(project.getBuildDir, name)

  // Should get the main sourceSet, but this is only available via JavaConventions...
  private def srcMain(project: Project): File = new File(project.getProjectDir, "src/main")

  def file(directory: File, name: String, extension: String): File = new File(directory, name + "." + extension)


  // TODO log suppressor
  //      // suppress output from the XSLT transforms - unless running with '-d' or '-i'.
  //      switch (project.gradle.startParameter.logLevel) {
  //        case LogLevel.DEBUG:
  //        case LogLevel.INFO:
  //        break;
  //        default:
  //          logging.captureStandardOutput(LogLevel.INFO)
  //        logging.captureStandardError(LogLevel.INFO)
  //      }
}
