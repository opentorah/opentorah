package org.opentorah.astronomy

import Angles.Rotation
import Days2Rotation.Days

class MoonHeadMeanTest extends Days2RotationTest(MoonHeadMean):
  behavior of "Mean Moon Head"

  it should "have printed values not exact" in:
    what.nonReconstructable.nonEmpty should be (true)
