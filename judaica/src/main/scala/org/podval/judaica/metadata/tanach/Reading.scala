package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan

final case class Reading(
  aliyot: Custom.Of[Seq[ChumashSpan.BookSpan]],
  maftir: Option[ChumashSpan.BookSpan],
  haftarah: Option[Haftarah]
)

object Reading {
  def apply(
    aliyot: Seq[ChumashSpan.BookSpan],
    maftir: Option[ChumashSpan.BookSpan],
    haftarah: Option[Haftarah]
  ): Reading = Reading(
    aliyot = Custom.ofCommon(aliyot),
    maftir = maftir,
    haftarah = haftarah
  )
}
