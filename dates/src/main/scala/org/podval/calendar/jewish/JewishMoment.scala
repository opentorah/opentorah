package org.podval.calendar.jewish

import org.podval.calendar.dates.MomentBase
import Jewish.Moment

abstract class JewishMoment(negative: Boolean, digits: Seq[Int])
  extends MomentBase[Jewish](negative, digits)
{
  final def nightHours(value: Int): Moment = firstHalfHours(value)

  final def dayHours(value: Int): Moment = secondHalfHours(value)
}
