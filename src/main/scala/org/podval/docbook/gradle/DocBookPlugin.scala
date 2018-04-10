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
import org.gradle.api.internal.artifacts.dependencies.{DefaultDependencyArtifact, DefaultExternalModuleDependency}
import org.gradle.api.tasks.Copy
import org.gradle.api.{Action, DefaultTask, Plugin, Project}

import scala.collection.JavaConverters._

final class DocBookPlugin extends Plugin[Project] {
  def apply(project: Project): Unit = {
    import DocBookPlugin._

    val extension: DocBookExtension = project.getExtensions.create("docbook", classOf[DocBookExtension], project)

    // The following is equivalent to:
    //   configurations { docbookxsl }
    //   dependencies { docbookxsl 'net.sf.docbook:docbook-xsl:1.79.1:resources@zip' }
    // TODO make version overridable in the project using this extension
    //   (best - in docbook{ xslVersion = ...}; worse - in dependencies {}, but "can't change after... resolved"...)
    // (like 'zinc' for scala-plugin).
    // v1.79.2 has been out for a while, but doesn't seem to have made it into Maven repositories...
    val docBookXslDependency = new DefaultExternalModuleDependency("net.sf.docbook", "docbook-xsl", "1.79.1")
    val docBookXslDependencyArtifact = new DefaultDependencyArtifact()
    docBookXslDependencyArtifact.setName("docbook-xsl")
    docBookXslDependencyArtifact.setClassifier("resources")
    docBookXslDependencyArtifact.setExtension("zip")
    docBookXslDependencyArtifact.setType("zip")
    docBookXslDependency.getArtifacts.add(docBookXslDependencyArtifact)
    project.getConfigurations.create(docBookXslConfiguration).getDependencies.add(docBookXslDependency)

    // TODO?        xslthl: 'net.sf.xslthl:xslthl:2.1.0'

    val explodeDocBookXslTask: Copy = copy(project, name = "explodeDocBookXsl",
      from = project.zipTree(explodeDocBookXslFrom(project)),
      into = explodeDocBookXslInto(project)
    )

    val copyDocBookTask: Copy = copy(project, name = "copyDocBook",
      from = project.fileTree(copyDocBookFrom(project)),
      into = copyDocBookInto(project),
      tokens = Map(
        "version" -> project.getVersion.toString,
        "date"    -> LocalDate.now().format(DateTimeFormatter.ofPattern("d MMM yyyy"))
      )
    )

    val copyXslTask: Copy = copy(project, name = "copyXsl",
      from = project.fileTree(copyXslFrom(project)),
      into = copyXslInto(project),
      tokens = Map(
        "docbook" -> docBookXsl(project).getAbsolutePath
      )
    )

    val prepareDocBookTask: DefaultTask = project.getTasks.create("prepareDocBook", classOf[DefaultTask])
    prepareDocBookTask.getDependsOn.add(explodeDocBookXslTask)
    prepareDocBookTask.getDependsOn.add(copyXslTask)
    prepareDocBookTask.getDependsOn.add(copyDocBookTask)

    val prepareDocBookDataTask: DefaultTask = project.getTasks.create("prepareDocBookData", classOf[DefaultTask])
    prepareDocBookDataTask.getDependsOn.add(prepareDocBookTask)

    val docBookHtmlTask: SaxonTask = SaxonTask(project,
      name = "docBookHtml",
      inputName = extension.getInputFileName,
      stylesheetName = "html",
      outputName = Some("index"),
      outputType = "html"
    )
    docBookHtmlTask.getDependsOn.add(prepareDocBookDataTask)

    val docBookFoTask: SaxonTask = SaxonTask(project,
      name = "docBookFo",
      inputName = extension.getInputFileName,
      stylesheetName = "pdf",
      outputType = "fo"
    )
    docBookFoTask.getDependsOn.add(prepareDocBookDataTask)

    // TODO move into FopTask.apply()
    val docBookPdfTask: FopTask = project.getTasks.create("docBookPdf", classOf[FopTask])
    docBookPdfTask.setInput(docBookFoTask.getOutput)
    val outputDirectory: File = new File(project.getBuildDir, "pdf")
    val output: File = new File(outputDirectory, "index.pdf")
    docBookPdfTask.setOutput(output)
    docBookPdfTask.getDependsOn.add(docBookFoTask)

    val processDocBookTask: DefaultTask = project.getTasks.create("processDocBook", classOf[DefaultTask])
    processDocBookTask.getDependsOn.add(docBookPdfTask)
    processDocBookTask.getDependsOn.add(docBookHtmlTask)
  }
}


// Task classes can not be 'final' - Gradle needs to create a proxy...
// TODO add task descriptors
object DocBookPlugin {
  def copy(
    project: Project,
    name: String,
    from: FileTree,
    into: File,
    tokens: Map[String, String] = Map.empty): Copy =
  {
    project.getTasks.create(name, classOf[Copy], new Action[Copy] {
      override def execute(copy: Copy): Unit = {
        copy.from(from)
        copy.into(into)
        copy.filter(Map("tokens" -> tokens.asJava).asJava, classOf[ReplaceTokens])
      }
    })
  }

  private val docBookXslConfiguration: String = "docbookxsl"

  // TODO from the PLUGIN's dependencies, not from the project's!!!
  // DO I really need to add the configuration and dependency to the project programmatically?!
  def explodeDocBookXslFrom(project: Project): File = project.getConfigurations.getByName(docBookXslConfiguration).getSingleFile

  def explodeDocBookXslInto(project: Project): File = new File(project.getBuildDir, docBookXslConfiguration)

  private val docBookXslRootDirectory: String = "docbook"
  def docBookXsl(project: Project): File = new File(explodeDocBookXslInto(project), docBookXslRootDirectory)

  private val xslDirName: String = "xsl"
  def copyXslFrom(project: Project): File = new File(srcMain(project), xslDirName)
  def copyXslInto(project: Project): File = new File(project.getBuildDir, xslDirName)
  def xslFile(project: Project, name: String): File = file(copyXslInto(project), name, "xsl")

  private val docBookDirName: String = "docbook"
  def copyDocBookFrom(project: Project): File = new File(srcMain(project), docBookDirName)
  def copyDocBookInto(project: Project): File = new File(project.getBuildDir, docBookDirName)

  // Should get the main sourceSet, but that isn't available from the project itself...
  private def srcMain(project: Project): File = new File(project.getProjectDir, "src/main")

  def fopConfiguration(project: Project): File = new File(srcMain(project), "fop/fop.xconf")

  def buildDirectory(project: Project, name: String): File = new File(project.getBuildDir, name)
  def file(directory: File, name: String, extension: String): File = new File(directory, name + "." + extension)
}
