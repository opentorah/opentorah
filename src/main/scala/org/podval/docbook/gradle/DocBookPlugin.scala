package org.podval.docbook.gradle

import org.gradle.api.{Action, DefaultTask, Plugin, Project}
import org.gradle.api.tasks.Copy
//import org.apache.tools.ant.filters.ReplaceTokens
//import org.gradle.api.file.FileTree
//import scala.collection.JavaConverters._
import java.io.File

final class DocBookPlugin extends Plugin[Project] {
  def apply(project: Project): Unit = {

    val extension: DocBookExtension = project.getExtensions.create("docbook", classOf[DocBookExtension], project)

    val docBookXslConfiguration = project.getConfigurations.create("docbookxsl").defaultDependencies(
      _.add(project.getDependencies.create("net.sf.docbook:docbook-xsl:1.79.1:resources@zip")) : Unit
    ).setVisible(false)

    val prepareDocBookTask: Copy = project.getTasks.create("prepareDocBook", classOf[Copy], new Action[Copy] {
      override def execute(task: Copy): Unit = {
        task.setDescription("Prepare DocBook XSLT stylesheets")
        task.from(project.zipTree(docBookXslConfiguration.getSingleFile))
        task.into(Locations.explodeDocBookXslInto(project))
        //      copy.filter(Map("tokens" -> tokens.asJava).asJava, classOf[ReplaceTokens])
        ()
      }
    })

    val prepareDocBookDataTask: DefaultTask = project.getTasks.create("prepareDocBookData", classOf[DefaultTask])
    prepareDocBookDataTask.setDescription("Generate data for inclusion in DocBook; placeholder task: attach via doLast{}")
    prepareDocBookDataTask.getDependsOn.add(prepareDocBookTask)

    val docBookHtmlTask: SaxonTask = SaxonTask(project,
      name = "docBookHtml",
      description = "DocBook -> HTML",
      inputFileName = extension.getInputFileName,
      stylesheetName = "html",
      xslParameters = extension.xslParameters,
      dataDirectory = extension.getDataDirectory,
      imagesDirectory = extension.imagesDirectory,
      outputFileNameOverride = Some("index"),
      outputType = "html"
    )
    val copyImagesTask: Copy = project.getTasks.create("copyImages", classOf[Copy], new Action[Copy] {
      override def execute(task: Copy): Unit = {
        task.setDescription("Copy images into the HTML generated from DocBook")
        task.from(Locations.imagesDirectory(project))
        task.into(new File(Locations.outputDirectory(project, "html"), "images"))
        ()
      }
    })
    docBookHtmlTask.getDependsOn.add(prepareDocBookDataTask)
    docBookHtmlTask.getDependsOn.add(copyImagesTask)

    // TODO copy the CSS - with filtering.
    // copy { from "src/main/css"    into "$outputDirectory/css"    }

    val docBookFoTask: SaxonTask = SaxonTask(project,
      name = "docBookFo",
      description = "DocBook -> XSL-FO",
      inputFileName = extension.getInputFileName,
      stylesheetName = "pdf",
      xslParameters = extension.xslParameters,
      dataDirectory = extension.getDataDirectory,
      imagesDirectory = extension.imagesDirectory,
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
    processDocBookTask.setGroup("publishing")
    processDocBookTask.getDependsOn.add(docBookPdfTask)
    processDocBookTask.getDependsOn.add(docBookHtmlTask)
  }
}
