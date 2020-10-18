package org.opentorah.dates

import org.opentorah.numbers.PointCompanion

trait MomentCompanion[C <: Calendar[C]] extends PointCompanion[C] with CalendarMember[C]
