package org.podval.docbook.gradle

import org.podval.docbook.gradle.node.Distribution
import org.podval.docbook.gradle.util.Platform
import org.scalatest.{FlatSpec, Matchers}

class NodeTest extends FlatSpec with Matchers {

  it should "calculate bin directory correctly" in {
    val distribution: Distribution = new Distribution("10.15.3", Platform.getOs, Platform.getArch)
    distribution.hasBinSubdirectory shouldBe true
  }
}
