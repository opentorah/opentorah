package org.opentorah.angles

import org.opentorah.numbers.{Digits, PointNumber}

abstract class PositionAngle(angles: Angles, digits: Digits)
  extends PointNumber[Angles](angles, digits) with Angle[PositionAngle]
