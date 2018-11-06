package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.LanguageSpec
import org.podval.judaica.metadata.tanach.BookSpan.{ChumashSpan, ProphetSpan}
import org.podval.judaica.metadata.tanach.Custom

final class Reading private(val customs: Custom.Of[Reading.ReadingCustom]) {
  override def toString: String = toString(LanguageSpec.empty)

  def toString(spec: LanguageSpec): String =
    customs.toSeq.map { case (custom, readingCustom) =>
      custom.toString(spec) + ": " + readingCustom.toString(spec)
    }.mkString("\n")
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
  }

  final case class MaftirAndHaftarah(
    maftir: ChumashSpan.BookSpan,
    haftarah: Seq[ProphetSpan.BookSpan]
  ) {
    def toString(spec: LanguageSpec): String =
      "maftir: " + maftir.toString(spec) + " haftarah: " + ProphetSpan.toString(haftarah, spec)
  }

  def apply(torah: Torah, maftir: ChumashSpan.BookSpan, haftarah: Haftarah.Customs): Reading =
    apply(Custom.ofCommon(torah), maftir, haftarah)

  def apply(torah: Custom.Of[Torah], maftir: ChumashSpan.BookSpan, haftarah: Haftarah.Customs): Reading = {
    // TODO handle the case when there are Torah customs not represented in Haftarah
    val keys: Set[Custom] = haftarah.keySet
    val customs = keys.map { custom =>
      val torahCustom = torah(Custom.find(torah, custom))
      val haftarahCustom = haftarah(Custom.find(haftarah, custom))
      custom -> ReadingCustom(
        torah = torahCustom,
        maftirAndHaftarah = Some(MaftirAndHaftarah(maftir = maftir, haftarah = haftarahCustom))
      )
    }

    // TODO simplify

    new Reading(customs.toMap)
  }

  def mkOptional(torah: Torah, haftarah: Haftarah.OptionalCustoms): Reading = {
    val customs = haftarah.mapValues { _.fold(ReadingCustom(torah, None)) { haftarah =>
      ReadingCustom(torah.init, Some(MaftirAndHaftarah(torah.last, haftarah)))
    }}

    new Reading(customs)
  }

  def apply(torah: Torah, haftarah: Haftarah.Customs): Reading = apply(
    torah = torah.init,
    maftir = torah.last,
    haftarah = haftarah
  )

  def apply(torah: Torah): Reading = Reading(Custom.ofCommon(torah))

  def apply(torah: Custom.Of[Torah]): Reading = {
    val customs = torah.mapValues(torah => ReadingCustom(torah, maftirAndHaftarah = None))
    new Reading(customs)
  }
}
