package org.opentorah.texts.tanach

import org.opentorah.metadata.{Named, Names}
import org.opentorah.texts.tanach.Parsha.{Mattos, Nitzavim}

final case class WeeklyReading(parsha: Parsha, secondParsha: Option[Parsha]) extends Named:
  def isCombined: Boolean = secondParsha.isDefined

  override def names: Names = if !isCombined then parsha.names else
    Names.combine(parsha.names, secondParsha.get.names, (_, one, other) =>
        one + "-" + other
    )

  def getMorningReading: Reading =
    val haftarahParsha = if isCombined && (parsha != Mattos) && (parsha != Nitzavim) then secondParsha.get else parsha
    Reading(
      torah = (if isCombined then parsha.daysCombined.get else parsha.days).map(_.fromWithNumbers(this)),
      maftir = Some((if isCombined then secondParsha.get else parsha).maftir.from(this)),
      haftarah = haftarahParsha.haftarah
    )

  def getAfternoonReading: Reading =
    Reading(torah = parsha.aliyot.from(this))
