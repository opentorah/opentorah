package org.podval.calendar.tanach

import org.podval.judaica.metadata.Names
import org.podval.judaica.tanach.{Custom, Torah}
import org.podval.judaica.tanach.Torah.Maftir

final class Reading(customs: Map[Custom, Reading.ReadingCustom], val names: Option[Names] = None)
  extends Custom.Of[Reading.ReadingCustom](customs)
{
  def torah: Torah.Customs =
    new Custom.Of(customs.mapValues(_.torah)).minimize

  def maftirAndHaftarah: Custom.Of[Option[Reading.MaftirAndHaftarah]] =
    new Custom.Of(customs.mapValues(_.maftirAndHaftarah)).minimize

  def maftir: Custom.Of[Option[Maftir]] =
    new Custom.Of(customs.mapValues(_.maftirAndHaftarah.map(_.maftir))).minimize

  def haftarah: Custom.Of[Option[Haftarah]] =
    new Custom.Of(customs.mapValues(_.maftirAndHaftarah.map(_.haftarah))).minimize

  def transformTorah(transformer: Torah => Torah): Reading =
    new Reading(customs.mapValues { reading =>
      reading.copy(torah = transformer(reading.torah))
    })

  def transform[Q](
    haftarah: Custom.Of[Q],
    transformer: (Custom, Reading.ReadingCustom, Q) => Reading.ReadingCustom
  ): Reading = new Reading(liftLR[Q, Reading.ReadingCustom](haftarah, transformer).customs)
}

object Reading {
  final case class ReadingCustom(
    torah: Torah,
    maftirAndHaftarah: Option[MaftirAndHaftarah]
  ) {
    def addHaftarah(haftaraAddition: Haftarah): ReadingCustom =
      copy(maftirAndHaftarah = Some(maftirAndHaftarah.get.addHaftarah(haftaraAddition)))

    def replaceMaftirAndHaftarah(maftir: Maftir, haftarah: Haftarah): ReadingCustom =
      copy(maftirAndHaftarah = Some(MaftirAndHaftarah(maftir, haftarah)))

    def replaceHaftarah(haftarah: Haftarah): ReadingCustom =
      copy(maftirAndHaftarah = Some(maftirAndHaftarah.get.replaceHaftarah(haftarah)))
  }

  final case class MaftirAndHaftarah(
    maftir: Maftir,
    haftarah: Haftarah
  ) {
    def addHaftarah(haftaraAddition: Haftarah): MaftirAndHaftarah =
      copy(haftarah = haftarah ++ haftaraAddition)

    def replaceHaftarah(haftarah: Haftarah): MaftirAndHaftarah =
      copy(haftarah = haftarah)
  }

  def apply(torah: Torah, names: Option[Names] = None): Reading =
    apply(Custom.Of(torah), names = names)

  def apply(torah: Torah.Customs, names: Option[Names]): Reading = {
    val result: Map[Custom, ReadingCustom] =
      torah.customs.mapValues(torah => ReadingCustom(torah, maftirAndHaftarah = None))
    new Reading(result, names)
  }

  def apply(torah: Torah, haftarah: Haftarah.Customs): Reading =
    apply(torah = Torah(torah.spans.init), maftir = torah.spans.last, haftarah = haftarah)

  def apply(torah: Torah, maftir: Maftir, haftarah: Haftarah.Customs): Reading =
    apply(Custom.Of(torah), maftir, haftarah, names = None)

  def apply(torah: Torah.Customs, maftir: Maftir, haftarah: Haftarah.Customs, names: Option[Names]): Reading = {
    val result: Map[Custom, ReadingCustom] =
      torah.liftLR[Haftarah, ReadingCustom](haftarah, { case (_: Custom, torah: Torah, haftarah: Haftarah) =>
        ReadingCustom(torah, maftirAndHaftarah = Some(MaftirAndHaftarah(maftir, haftarah)))
      }).customs

    new Reading(result, names)
  }
}
