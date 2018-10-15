package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan

final case class Reading(
  aliyot: Custom.Of[Seq[ChumashSpan.BookSpan]],
  maftir: Option[ChumashSpan.BookSpan],
  haftarah: Option[Haftarah]
)

object Reading {
  def apply(
    aliyot: SpecialReading,
    maftir: SpecialReading,
    haftarah: SpecialReading
  ): Reading = Reading(
    aliyot = Custom.ofCommon(aliyot.weekdayAliyot.get.getAliyot),
    maftir = Some(maftir.maftir.get),
    haftarah = Some(haftarah.haftarah.get)
  )

  def apply(
    aliyot: Aliyot,
    maftir: SpecialReading,
    haftarah: SpecialReading
  ): Reading = Reading(
    aliyot = Custom.ofCommon(aliyot.getAliyot),
    maftir = Some(maftir.maftir.get),
    haftarah = Some(haftarah.haftarah.get)
  )

  def apply(
    aliyot: Seq[ChumashSpan.BookSpan],
    maftir: SpecialReading,
    haftarah: SpecialReading
  ): Reading = Reading(
    aliyot = Custom.ofCommon(aliyot),
    maftir = Some(maftir.maftir.get),
    haftarah = Some(haftarah.haftarah.get)
  )
}
