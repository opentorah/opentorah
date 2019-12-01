package org.podval.docbook.gradle

import org.podval.fop.mathjax.NodeDistribution
import org.podval.fop.util.Platform
import org.scalatest.{FlatSpec, Matchers}

class NodeDistributionTest extends FlatSpec with Matchers {

  it should "calculate bin directory correctly" in {
    val distribution: NodeDistribution = new NodeDistribution(Platform.getOs, Platform.getArch)
    distribution.hasBinSubdirectory shouldBe true
  }
}
