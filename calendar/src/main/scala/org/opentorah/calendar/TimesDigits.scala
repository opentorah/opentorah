package org.opentorah.calendar

import org.opentorah.numbers.DigitsDescriptor

object TimesDigits extends DigitsDescriptor:
  object DAYS extends DigitBase("d")
  object HOURS extends DigitBase("h")
  object PARTS extends DigitBase("p")
  object MOMENTS extends DigitBase("m")

  override val values: Seq[DigitsDescriptor.Digit] = Seq(DAYS, HOURS, PARTS, MOMENTS)
