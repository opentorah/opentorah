package org.opentorah.texts.tanach

import org.opentorah.metadata.{Named, Names}

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
    // A combined week follows its second parsha, except for the customs whose
    // first parsha claims precedenceWhenCombined -- the whole tree for
    // Nitzavim-Vayeilech, Chabad alone for Acharei-Kedoshim. Mattos-Masei used
    // to be excepted here too, which dropped Masei's haftarah -- see
    // Readings.correctPinchas.
    val haftarah: Haftarah.Customs =
      if !isCombined then parsha.haftarah
      else secondParsha.get.haftarah.graft(parsha.haftarah, Haftarah.precedenceWhenCombined(parsha))
    Reading(
      torah = (if isCombined then parsha.daysCombined.get else parsha.days).map(_.fromWithNumbers(this)),
      maftir = Some((if isCombined then secondParsha.get else parsha).maftir.from(this)),
      haftarah = haftarah
    )

  def getAfternoonReading: Reading =
    Reading(torah = parsha.aliyot.from(this))
