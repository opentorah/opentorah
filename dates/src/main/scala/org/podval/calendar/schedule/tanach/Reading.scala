package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan
import org.podval.judaica.metadata.tanach.Custom

// TODO modify Reading to apply to a single custom; eliminate Option[] from here.
final case class Reading(
  aliyot: Custom.Of[Seq[ChumashSpan.BookSpan]],
  maftir: Option[ChumashSpan.BookSpan],
  haftarah: Option[Haftarah.OptionalCustoms]
)

object Reading {
  def apply(
    aliyot: Seq[ChumashSpan.BookSpan],
    maftir: Option[ChumashSpan.BookSpan],
    haftarah: Option[Haftarah.OptionalCustoms]
  ): Reading = Reading(
    aliyot = Custom.ofCommon(aliyot),
    maftir = maftir,
    haftarah = haftarah
  )
}
