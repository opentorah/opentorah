package org.opentorah.math

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class NodeDistributionTest extends AnyFlatSpecLike, Matchers:

  it should "calculate bin directory correctly" in {
    val distribution: NodeDistribution = NodeDistribution(version = "14.1.0")
    distribution.hasBinSubdirectory shouldBe true
  }
