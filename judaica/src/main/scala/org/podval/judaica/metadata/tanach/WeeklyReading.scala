package org.podval.judaica.metadata.tanach

final case class WeeklyReading(parsha: Parsha, secondParsha: Option[Parsha]) {
  def isCombined: Boolean = secondParsha.isDefined

  def getReading: Reading = Reading(
    aliyot = (if (isCombined) parsha.daysCombined.get else parsha.days).mapValues(_.getAliyot),
    maftir = Some((if (isCombined) secondParsha.get else parsha).maftir),
    // TODO when is it the first haftarah and when is it the second one?
    haftarah = Some((if (isCombined) secondParsha.get else parsha).haftarah)
  )
}
