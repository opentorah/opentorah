package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.LanguageSpec
import org.podval.judaica.metadata.tanach.BookSpan.{ChumashSpan, ProphetSpan}
import org.podval.judaica.metadata.tanach.Custom

final class Reading(map: Map[Custom, Reading.ReadingCustom]) extends Custom.Of[Reading.ReadingCustom](map) {
  override def toString: String = toString(LanguageSpec.empty)

  def toString(spec: LanguageSpec): String =
    customs.toSeq.map { case (custom, readingCustom) =>
      custom.toString(spec) + ": " + readingCustom.toString(spec)
    }.mkString("\n")

  def transformTorah(transformer: Reading.Torah => Reading.Torah): Reading =
    new Reading(customs.mapValues { reading =>
      reading.copy(torah = transformer(reading.torah))
    })

  def transform[Q](
    haftarah: Custom.Of[Q],
    transformer: (Custom, Reading.ReadingCustom, Option[Q]) => Reading.ReadingCustom
  ): Reading = new Reading(liftL[Q, Reading.ReadingCustom](haftarah, transformer))

  def transformR[Q](
    haftarah: Custom.Of[Q],
    transformer: (Custom, Reading.ReadingCustom, Q) => Reading.ReadingCustom
  ): Reading = new Reading(liftLR[Q, Reading.ReadingCustom](haftarah, transformer))
}

object Reading {
  type Torah = Seq[ChumashSpan.BookSpan]
  type Haftarah = Seq[ProphetSpan.BookSpan]

  final case class ReadingCustom(
    torah: Torah,
    maftirAndHaftarah: Option[MaftirAndHaftarah]
  ) {
    def toString(spec: LanguageSpec): String =
      "torah: " + ChumashSpan.toString(torah, spec) + maftirAndHaftarah.fold("")(it => " " + it.toString(spec))

    def addHaftarah(haftaraAddition: Haftarah.Haftarah): ReadingCustom =
      copy(maftirAndHaftarah = Some(maftirAndHaftarah.get.addHaftarah(haftaraAddition)))

    def replaceMaftirAndHaftarah(maftir: ChumashSpan.BookSpan, haftarah: Haftarah.Haftarah): ReadingCustom =
      copy(maftirAndHaftarah = Some(MaftirAndHaftarah(maftir, haftarah)))

    def replaceHaftarah(haftarah: Haftarah.Haftarah): ReadingCustom =
      copy(maftirAndHaftarah = Some(maftirAndHaftarah.get.replaceHaftarah(haftarah)))
  }

  final case class MaftirAndHaftarah(
    maftir: ChumashSpan.BookSpan,
    haftarah: Seq[ProphetSpan.BookSpan]
  ) {
    def toString(spec: LanguageSpec): String =
      "maftir: " + maftir.toString(spec) + " haftarah: " + ProphetSpan.toString(haftarah, spec)

    def addHaftarah(haftaraAddition: Haftarah.Haftarah): MaftirAndHaftarah =
      copy(haftarah = haftarah ++ haftaraAddition)

    def replaceHaftarah(haftarah: Haftarah.Haftarah): MaftirAndHaftarah =
      copy(haftarah = haftarah)
  }

  def apply(torah: Torah): Reading = apply(Custom.Of(torah))

  def apply(torah: Custom.Of[Torah]): Reading =
    new Reading(torah.customs.mapValues(torah => ReadingCustom(torah, maftirAndHaftarah = None)))

  def apply(torah: Torah, haftarah: Haftarah.Customs): Reading = apply(
    torah = torah.init,
    maftir = torah.last,
    haftarah = haftarah
  )

  def apply(torah: Torah, maftir: ChumashSpan.BookSpan, haftarah: Haftarah.Customs): Reading =
    apply(Custom.Of(torah), maftir, haftarah)

  def apply(torah: Custom.Of[Torah], maftir: ChumashSpan.BookSpan, haftarah: Haftarah.Customs): Reading = new Reading(
    torah.liftLR[Haftarah.Haftarah, ReadingCustom](haftarah, { case (custom: Custom, torah: Torah, haftarah: Haftarah.Haftarah) =>
      ReadingCustom(torah, maftirAndHaftarah = Some(MaftirAndHaftarah(maftir, haftarah)))
    })
  )
}
