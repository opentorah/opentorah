package org.opentorah.dates

import org.opentorah.numbers.PointCompanion

abstract class MomentCompanion[C <: Calendar[C]](calendar: C)
  extends PointCompanion[C](calendar) with CalendarMember[C]
