package org.opentorah.astronomy

import Days2Rotation.Days

class SunApogeeTest extends Days2RotationTest(SunApogee):

  behavior of "Sun Apogee"

  // value is too small
  it should "have printed values exact" in {
    what.nonReconstructable.nonEmpty should be (false)
  }
  