package org.opentorah.astronomy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ZodiacTest  extends AnyFlatSpec, Matchers:
  
  "Zodiacs" should "load correctly" in {
    Zodiac.Aries.names.hasName("Овен") shouldBe true
  }

