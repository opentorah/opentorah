package org.opentorah.texts.tanach

import org.opentorah.metadata.{Names, WithNames}
import org.opentorah.texts.tanach.Parsha.{Mattos, Nitzavim}

final case class WeeklyReading(parsha: Parsha, secondParsha: Option[Parsha]) extends WithNames {
  def isCombined: Boolean = secondParsha.isDefined

  override def names: Names = if (!isCombined) parsha.names else
    Names.combine(parsha.names, secondParsha.get.names, { case (_, one, other) =>
        one + "-" + other
    })

  def getMorningReading: Reading = {
    val haftarahParsha = if (isCombined && (parsha != Mattos) && (parsha != Nitzavim)) secondParsha.get else parsha
    Reading(
      torah = (if (isCombined) parsha.daysCombined.get else parsha.days).map(_.fromWithNumbers(this)),
      maftir = Some((if (isCombined) secondParsha.get else parsha).maftir.from(this)),
      haftarah = Haftarah.forParsha(haftarahParsha)
    )
  }

  def getAfternoonReading: Reading =
    Reading(torah = parsha.aliyot.from(this))
}
