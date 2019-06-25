package org.podval.docbook.gradle

import java.io.File

import org.gradle.testkit.runner.GradleRunner
import org.podval.docbook.gradle.plugin.{DocBook, Layout, Write}
import org.podval.docbook.gradle.util.Util
import org.podval.docbook.gradle.xml.Xml

class PluginTestProject(projectDir: File) {
  projectDir.mkdirs()

  private val write: Write = new Write(
    layout = Layout.forRoot(projectDir),
    logger = new TestLogger)

  def layout: Layout = write.layout

  def destroy(): Unit = Util.deleteRecursively(projectDir)

  def run(logInfo: Boolean = false): String = getRunner(logInfo).build.getOutput

  def fail(): String = getRunner(logInfo = false).buildAndFail.getOutput

  def indexHtml: String = saxonOutputFile(section.Html)

  def fo: String = saxonOutputFile(section.Pdf)

  private def saxonOutputFile(docBook2: section.DocBook2): String =
    Util.readFrom(layout.forDocument(prefixed = false, PluginTestProject.documentName).saxonOutputFile(docBook2))

  private def getRunner(logInfo: Boolean): GradleRunner = {
    val result = GradleRunner.create.withProjectDir(projectDir)
    if (logInfo) result.withArguments("-i", "processDocBook") else result.withArguments("processDocBook")
  }
}

object PluginTestProject {

  private val documentName: String = "test"

  def apply(
    name: String,
    prefix: Option[String] = None,
    document: String = s"<article ${DocBook.Namespace.withVersion}/>",
    substitutions: Map[String, String] = Map.empty,
    isPdfEnabled: Boolean = false,
    isMathJaxEnabled: Boolean = false,
    useJ2V8: Boolean = false,
  ): PluginTestProject = {
    val layout: Layout = Layout.forCurrent
    val projectDir: File = new File(Util.prefixedDirectory(layout.buildDir, prefix), name)
    val result: PluginTestProject = new PluginTestProject(projectDir)

    result.write.settingsGradle(layout.projectDir)

    val substitutionsFormatted: String = if (substitutions.isEmpty) "" else {
      val contents: String = substitutions.map { case (name: String, value: String) =>
        s""""$name": $value"""
      }.mkString(",\n")

      s"""
         |  substitutions = [
         |    $contents
         |  ]
         |"""
    }

    val outputFormats: String = if (isPdfEnabled) """ "html", "pdf" """ else """ "html" """

    result.write.buildGradle(content =
      s"""|plugins {
          |  id 'org.podval.docbook-gradle-plugin' version '1.0.0'
          |  id 'base'
          |}
          |
          |repositories {
          |  jcenter()
          |}
          |
          |docBook {
          |  document = "$documentName"
          |  outputFormats = [$outputFormats]
          |$substitutionsFormatted
          |
          |  mathJax {
          |    isEnabled = $isMathJaxEnabled
          |    useJ2V8 = $useJ2V8
          |  }
          |}
          |"""
    )

    result.write.inputFile(documentName, content = s"${Xml.header}\n$document")

    result
  }
}
