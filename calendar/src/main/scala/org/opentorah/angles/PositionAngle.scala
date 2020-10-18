package org.opentorah.angles

import org.opentorah.numbers.{Digits, PointNumber}

abstract class PositionAngle(digits: Digits) extends PointNumber[Angles](digits) with Angle[PositionAngle]
