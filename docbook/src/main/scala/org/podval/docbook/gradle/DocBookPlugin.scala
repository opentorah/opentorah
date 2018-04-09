/*
 * Inspired by a Gist by Aristedes Maniatis
 * https://gist.github.com/ari/4156d967d54289f4abf6
 */
package org.podval.docbook.gradle

import java.io.File
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import org.apache.tools.ant.filters.ReplaceTokens
import org.gradle.api.file.FileTree
import org.gradle.api.provider.Property
import org.gradle.api.tasks.{Copy, TaskAction}
import org.gradle.api.{DefaultTask, Plugin, Project}

import scala.collection.JavaConverters._

final class DocBookPlugin extends Plugin[Project] {
  def apply(project: Project): Unit = {
    import DocBookPlugin._

    val extension: DocBookExtension = project.getExtensions.create("docbook", classOf[DocBookExtension], project)

    val explodeDocBookXslTask: Copy = project.getTasks.create("explodeDocBookXsl", classOf[Copy])
    explodeDocBookXslTask.from(explodeDocBookXslFrom(project))
    explodeDocBookXslTask.into(explodeDocBookXslInto(project))

    val copyXslTask: Copy = project.getTasks.create("copyXsl", classOf[Copy])
    copyXslTask.from(copyXslFrom(project))
    copyXslTask.into(copyXslInto(project))
    val xslTokens: Map[String, String] = Map(
      "docbook" -> docbookXsl(project).getAbsolutePath
    )
    copyXslTask.filter(Map("tokens" -> xslTokens.asJava).asJava, classOf[ReplaceTokens])

    val copyDocBookTask: Copy = project.getTasks.create("copyDocBook", classOf[Copy])
    copyDocBookTask.from(copyDocBookFrom(project))
    copyDocBookTask.into(copyDocBookInto(project))
    val docBookTokens: Map[String, String] = Map(
      "version" -> project.getVersion.toString,
      "date"    -> LocalDate.now().format(DateTimeFormatter.ofPattern("d MMM yyyy"))
    )
    copyDocBookTask.filter(Map("tokens" -> xslTokens.asJava).asJava, classOf[ReplaceTokens])

    val prepareDocBookTask: DefaultTask = project.getTasks.create("prepareDocBook", classOf[DefaultTask])
    prepareDocBookTask.getDependsOn.add(explodeDocBookXslTask)
    prepareDocBookTask.getDependsOn.add(copyXslTask)
    prepareDocBookTask.getDependsOn.add(copyDocBookTask)


    val processDocBookTask: ProcessDocBookTask =
      project.getTasks.create("processDocBook", classOf[ProcessDocBookTask], (docBookTask: ProcessDocBookTask) => {
        // TODO check that extension.getInputFile exists and is a file
        docBookTask.inputFile.set(extension.getInputFile)
      })

    processDocBookTask.getDependsOn.add(prepareDocBookTask)
  }
}


// Task classes can not be 'final' - Gradle needs to create a proxy...

object DocBookPlugin {
  class ProcessDocBookTask extends DefaultTask {
    val inputFile: Property[File] = getProject.getObjects.property(classOf[File])

    @TaskAction
    def process(): Unit = {
      println("********* inputFile= " + inputFile.get())
    }
  }


  private val docbookXslConiguration: String = "docbookxslt"

  // TODO from the PLUGIN's dependencies, not from the project's!!!
  // DO I really need to add the configuration and dependency to the project programmatically?!
  def explodeDocBookXslFrom(project: Project): FileTree =
    project.zipTree(project.getConfigurations.getByName(docbookXslConiguration).getSingleFile)

  private val docbookXsltArchive: String = "docbookXsl"
  def explodeDocBookXslInto(project: Project): File = new File(project.getBuildDir, docbookXsltArchive)

  private val docbookXslRootDirectory: String = "docbook"
  def docbookXsl(project: Project): File = new File(explodeDocBookXslInto(project), docbookXslRootDirectory)

  private val xslDirName: String = "xsl"
  def copyXslFrom(project: Project): File = new File(srcMain(project), xslDirName)
  def copyXslInto(project: Project): File = new File(project.getBuildDir, xslDirName)

  private val docBookDirName: String = "docbook"
  def copyDocBookFrom(project: Project): File = new File(srcMain(project), docBookDirName)
  def copyDocBookInto(project: Project): File = new File(project.getBuildDir, docBookDirName)

  // Should get the main sourceSet, but that isn't available from the project itself...
  private def srcMain(project: Project): File = new File(project.getProjectDir, "src/main")

  //  new File(project.getProjectDir().getAbsolutePath() + '/src/main/fop/fop.xconf')
}
