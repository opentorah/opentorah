package org.opentorah.texts.tanach

import org.opentorah.metadata.{Named, Names}
import org.opentorah.texts.tanach.Parsha.{Mattos, Nitzavim}

final class WeeklyReading(val parsha: Parsha, val secondParsha: Option[Parsha]) extends Named derives CanEqual:
  override def equals(other: Any): Boolean =
    val that: WeeklyReading = other.asInstanceOf[WeeklyReading]
    (this.parsha == that.parsha) && (this.secondParsha == that.secondParsha)

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
