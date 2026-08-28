package org.opentorah.texts.tanach

import org.opentorah.metadata.{Named, Names}
import org.opentorah.texts.tanach.Parsha.Nitzavim

final class WeeklyReading(val parsha: Parsha, val secondParsha: Option[Parsha]) extends Named derives CanEqual:
  override def equals(other: Any): Boolean = other match
    case that: WeeklyReading =>
      (this.parsha == that.parsha) && (this.secondParsha == that.secondParsha)
    case _ => false

  def isCombined: Boolean = secondParsha.isDefined

  override def names: Names = if !isCombined then parsha.names else
    Names.combine(parsha.names, secondParsha.get.names, (_, one, other) =>
        one + "-" + other
    )

  def getMorningReading: Reading =
    // Nitzavim-Vayeilech keeps Nitzavim's haftarah; every other combined
    // week follows its second parsha. Mattos-Masei used to be excepted here
    // too, which dropped Masei's haftarah -- see Readings.correctPinchas.
    val haftarahParsha = if isCombined && (parsha != Nitzavim) then secondParsha.get else parsha
    Reading(
      torah = (if isCombined then parsha.daysCombined.get else parsha.days).map(_.fromWithNumbers(this)),
      maftir = Some((if isCombined then secondParsha.get else parsha).maftir.from(this)),
      haftarah = haftarahParsha.haftarah
    )

  def getAfternoonReading: Reading =
    Reading(torah = parsha.aliyot.from(this))
