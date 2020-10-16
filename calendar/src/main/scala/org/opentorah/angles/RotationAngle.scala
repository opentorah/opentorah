package org.opentorah.angles

import org.opentorah.numbers.{Digits, VectorNumber}

abstract class RotationAngle(angles: Angles, digits: Digits)
  extends VectorNumber[Angles](angles, digits) with Angle[RotationAngle]
