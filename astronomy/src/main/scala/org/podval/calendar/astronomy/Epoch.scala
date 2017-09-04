package org.podval.calendar.astronomy

import org.podval.calendar.jewish.Jewish
import Jewish.{Year, Month, Day}

object Epoch {
  val epoch: Day = Year(4938).month(Month.Name.Nisan).day(3)
}
