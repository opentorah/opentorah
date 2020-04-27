package org.podval.fop.mathjax

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class NodeDistributionTest extends AnyFlatSpecLike with Matchers {

  it should "calculate bin directory correctly" in {
    val distribution: NodeDistribution = new NodeDistribution(version = NodeDistribution.defaultVersion)
    distribution.hasBinSubdirectory shouldBe true
  }
}
