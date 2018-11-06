package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.tanach.BookSpan.{ChumashSpan, ProphetSpan}
import org.podval.judaica.metadata.tanach.Custom

final class Reading(val customs: Custom.Of[Reading.ReadingCustom])

object Reading {
  type Torah = Seq[ChumashSpan.BookSpan]
  type Haftarah = Seq[ProphetSpan.BookSpan]

  final case class ReadingCustom(
    torah: Torah,
    maftirAndHaftarah: Option[MaftirAndHaftarah]
  )

  final case class MaftirAndHaftarah(
    maftir: ChumashSpan.BookSpan,
    haftarah: Seq[ProphetSpan.BookSpan]
  )

  // TODO eliminate the flavor with optional parameters!
  def apply(
    torah: Torah,
    maftir: Option[ChumashSpan.BookSpan],
    haftarah: Option[Haftarah.Customs]
  ): Reading = ???

  def apply(
    torah: Custom.Of[Torah],
    maftir: ChumashSpan.BookSpan,
    haftarah: Custom.Of[Haftarah]
  ): Reading = ??? // TODO merge customs

  def apply(torah: Torah): Reading = Reading(Custom.ofCommon(torah))

  def apply(torah: Custom.Of[Torah]): Reading = ??? //Reading(torah, None, None)
}
