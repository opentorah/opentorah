package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.LanguageSpec
import org.podval.judaica.metadata.tanach.BookSpan.{ChumashSpan, ProphetSpan}
import org.podval.judaica.metadata.tanach.Custom

// TODO make Reading INHERIT from Custom.Of[]!
final class Reading(val customs: Custom.Of[Reading.ReadingCustom]) {
  override def toString: String = toString(LanguageSpec.empty)

  def toString(spec: LanguageSpec): String =
    customs.customs.toSeq.map { case (custom, readingCustom) =>
      custom.toString(spec) + ": " + readingCustom.toString(spec)
    }.mkString("\n")

  def transformTorah(transformer: Reading.Torah => Reading.Torah): Reading =
    new Reading(new Custom.Of[Reading.ReadingCustom](customs.customs.mapValues { reading =>
      reading.copy(torah = transformer(reading.torah))
    }))

  def transform[Q](
    haftarah: Custom.Of[Q],
    transformer: (Custom, Reading.ReadingCustom, Option[Q]) => Reading.ReadingCustom
  ): Reading = new Reading(customs.liftL[Q, Reading.ReadingCustom](haftarah, transformer))

  def transformR[Q](
    haftarah: Custom.Of[Q],
    transformer: (Custom, Reading.ReadingCustom, Q) => Reading.ReadingCustom
  ): Reading = new Reading(customs.liftLR[Q, Reading.ReadingCustom](haftarah, transformer))
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

  def apply(torah: Torah, maftir: ChumashSpan.BookSpan, haftarah: Haftarah.Customs): Reading =
    apply(Custom.Of(torah), maftir, haftarah)

  def apply(torah: Custom.Of[Torah], maftir: ChumashSpan.BookSpan, haftarah: Haftarah.Customs): Reading = {
    // TODO handle the case when there are Torah customs not represented in Haftarah
    val keys: Set[Custom] = haftarah.keySet
    val customs = keys.map { custom =>
      val torahCustom = torah.doFind(custom)
      val haftarahCustom = haftarah.doFind(custom)
      custom -> ReadingCustom(
        torah = torahCustom,
        maftirAndHaftarah = Some(MaftirAndHaftarah(maftir = maftir, haftarah = haftarahCustom))
      )
    }

    new Reading(new Custom.Of(customs.toMap))
  }

  def apply(torah: Torah, haftarah: Haftarah.Customs): Reading = apply(
    torah = torah.init,
    maftir = torah.last,
    haftarah = haftarah
  )

  def apply(torah: Torah): Reading = Reading(Custom.Of(torah))

  def apply(torah: Custom.Of[Torah]): Reading = {
    val customs = torah.customs.mapValues(torah => ReadingCustom(torah, maftirAndHaftarah = None))
    new Reading(new Custom.Of(customs))
  }
}
