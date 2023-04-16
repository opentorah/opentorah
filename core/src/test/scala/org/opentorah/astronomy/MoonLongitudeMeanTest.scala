package org.opentorah.astronomy

import Angles.Rotation
import Days2Rotation.Days

class MoonLongitudeMeanTest extends Days2RotationTest(MoonLongitudeMean):
  behavior of "Mean Moon Longitude"

  it should "have printed values not exact" in:
    what.nonReconstructable.nonEmpty should be (true)
