package org.podval.docbook.gradle.node

import org.podval.docbook.gradle.util.Platform
import org.scalatest.{FlatSpec, Matchers}

class DistributionTest extends FlatSpec with Matchers  {

  "Distribution" should "calculate bin directory correctly" in {
    val distribution: Distribution = new Distribution("10.15.3", Platform.getOs, Platform.getArch)
    distribution.hasBinSubdirectory shouldBe true
  }
}
