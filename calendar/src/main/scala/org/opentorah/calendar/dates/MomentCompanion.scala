package org.opentorah.calendar.dates

import org.opentorah.numbers.PointCompanion

abstract class MomentCompanion[C <: Calendar[C]] extends PointCompanion[C] with CalendarMember[C]
