package org.podval.docbook.gradle.node

import org.podval.docbook.gradle.util.PluginTestProject
import org.scalatest.{FlatSpec, Matchers}

class NodeTest extends FlatSpec with Matchers {

  private val testProject: PluginTestProject = PluginTestProject(name = "nodeTestProject")

  "Node modules" should "be installed" in {
    testProject.run()
    testProject.layout.nodeModulesRoot.exists shouldBe true
  }
}
