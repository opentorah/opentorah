package org.podval.docbook.gradle

import org.gradle.testkit.runner.{BuildResult, GradleRunner}
import java.io.File

class PluginTestProject(
  val name: String,
  val document: String,
  val documentName: String = "test",
  val substitutions: Map[String, String] = Map.empty
) {
  val pluginDir: File = new File("build").getAbsoluteFile.getParentFile
  val projectDir: File = new File(s"build/pluginTestProject/$name").getAbsoluteFile
  val indexHtmlFile: File = new File(projectDir, "build/docBook/html/index.html").getAbsoluteFile

  val buildGradle: String =
    s"""plugins {
       |  id "org.podval.docbook-gradle-plugin" version "1.0.0"
       |}
       |
       |repositories {
       |  jcenter()
       |}
       |
       |apply plugin: 'base'
       |
       |docBook {
       |  document = "$documentName"
       |  outputFormats = ["html"]
       |  useRelativeBaseDir = true
       |$substitutionsFormatted
       |}
       |"""

  private def substitutionsFormatted: String = if (substitutions.isEmpty) "" else {
    val contents: String = substitutions.map { case (name: String, value: String) =>
      s""""$name": $value"""
    }.mkString(",\n")
    s"""
      |  substitutions = [
      |    $contents
      |  ]
      |"""
  }

  def destroy(): Unit = Util.deleteRecursively(projectDir)

  private def prepare(): Unit = {
    projectDir.mkdirs()

    writeInto("build.gradle",
      buildGradle)

    writeInto("settings.gradle",
      s"includeBuild '$pluginDir'")

    writeInto(s"src/main/docBook/$documentName.xml",
      """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" + document)
  }

  private val logger: Logger = new TestLogger

  private def writeInto(fileName: String, what: String): Unit =
    Util.writeInto(new File(projectDir, fileName), logger, replace = true)(what)

  def getIndexHtml: String = {
    val result: BuildResult = run
    indexHtml
  }

  def getIndexHtmlAndOutput: (String, String) = {
    val result: BuildResult = run
    (indexHtml, result.getOutput)
  }

  def fails: String =
    getRunner.buildAndFail.getOutput

  private def run: BuildResult =
    getRunner.build

  private def getRunner: GradleRunner = {
    prepare()

    GradleRunner.create
      .withProjectDir(projectDir)
      .withArguments("-i", "clean", "processDocBook")
  }

  private def indexHtml: String = Util.readFrom(indexHtmlFile)
}
