package org.opentorah.angles

import org.opentorah.numbers.{Digit, DigitsDescriptor}

object AnglesDigits extends DigitsDescriptor {
  object DEGREES extends DigitBase("°")
  object MINUTES extends DigitBase("′")
  object SECONDS extends DigitBase("″")
  object THIRDS  extends DigitBase("‴")

  override val values: Seq[Digit] = Seq(DEGREES, MINUTES, SECONDS, THIRDS)
}
