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
    isShabbos: Boolean,
    maftir: SpecialReading,
    haftarah: SpecialReading
  ): Reading = Reading(
    aliyot = aliyot.getAliyot(isShabbos).get.getAliyot,
    maftir = maftir,
    haftarah = haftarah
  )

  def apply(
    aliyot: Seq[ChumashSpan.BookSpan],
    maftir: SpecialReading,
    haftarah: SpecialReading
  ): Reading = Reading(
    aliyot = aliyot,
    maftir = maftir.maftir.get,
    haftarah = haftarah
  )

  def apply(
    aliyot: Seq[ChumashSpan.BookSpan],
    maftir: ChumashSpan.BookSpan,
    haftarah: SpecialReading
  ): Reading = Reading(
    aliyot = Custom.ofCommon(aliyot),
    maftir = Some(maftir),
    haftarah = Some(haftarah.haftarah.get)
  )

  def apply(aliyot: Seq[ChumashSpan.BookSpan]): Reading = Reading(
    aliyot = Custom.ofCommon(aliyot),
    maftir = None,
    haftarah = None
  )
}
