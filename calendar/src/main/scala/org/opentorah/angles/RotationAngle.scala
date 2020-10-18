package org.opentorah.angles

import org.opentorah.numbers.{Digits, VectorNumber}

abstract class RotationAngle(digits: Digits) extends VectorNumber[Angles](digits) with Angle[RotationAngle]
