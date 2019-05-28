package org.podval.docbook.gradle.node

import java.io.File

import org.podval.docbook.gradle.{Fixture, PluginTestProject}
import org.scalatest.{FlatSpec, Matchers}

class NodeTest extends FlatSpec with Matchers {

  private val testProject: PluginTestProject =
    new PluginTestProject(new File(Fixture.getBuildDir, "nodeTestProject"))

  testProject.writeSettingsGradle(Fixture.getProjectDir)
  testProject.writeBuildGradle()
  testProject.run("installMathJax")
  // TODO verify that MathJax module gets installed
}
