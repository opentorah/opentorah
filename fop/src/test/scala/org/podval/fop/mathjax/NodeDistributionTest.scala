package org.podval.fop.mathjax

import org.scalatest.{FlatSpec, Matchers}

class NodeDistributionTest extends FlatSpec with Matchers {

  it should "calculate bin directory correctly" in {
    val distribution: NodeDistribution = new NodeDistribution(version = NodeDistribution.defaultVersion)
    distribution.hasBinSubdirectory shouldBe true
  }
}
