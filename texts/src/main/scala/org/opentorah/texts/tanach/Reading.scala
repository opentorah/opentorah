package org.opentorah.texts.tanach

import org.opentorah.texts.tanach.Torah.Maftir
import org.opentorah.util.Collections.mapValues

final class Reading(customs: Custom.Customs[Reading.ReadingCustom])
  extends Custom.Of[Reading.ReadingCustom](customs):
  
  def torah: Torah.Customs = of(_.torah)

  def maftirAndHaftarah: Custom.Of[Option[Reading.MaftirAndHaftarah]] = of(_.maftirAndHaftarah)

  def maftir: Custom.Of[Option[Maftir]] = of(_.maftirAndHaftarah.flatMap(_.maftir))

  def haftarah: Custom.Of[Option[Haftarah]] = of(_.maftirAndHaftarah.map(_.haftarah))

  private def of[R](f: Reading.ReadingCustom => R)(using CanEqual[R, R]): Custom.Of[R] =
    Custom.Of(mapValues(customs)(f)).minimize

  def transformTorah(transformer: Torah => Torah): Reading =
    new Reading(mapValues(customs)((readingCustom: Reading.ReadingCustom) => Reading.ReadingCustom(
      torah = transformer(readingCustom.torah),
      maftirAndHaftarah = readingCustom.maftirAndHaftarah
    )))

  def transform[Q](
    haftarah: Custom.Of[Q],
    transformer: (Custom, Reading.ReadingCustom, Q) => Reading.ReadingCustom
  ): Reading = new Reading(liftLR[Q, Reading.ReadingCustom](haftarah, transformer).customs)

object Reading:
  // when there is no special maftir, last aliyah is the maftir
  final class MaftirAndHaftarah(
    val maftir: Option[Maftir],
    val haftarah: Haftarah
  ) derives CanEqual:
    override def equals(other: Any): Boolean =
      val that: MaftirAndHaftarah = other.asInstanceOf[MaftirAndHaftarah]
      (this.maftir == that.maftir) && (this.haftarah == that.haftarah)

  final class ReadingCustom(
    val torah: Torah,
    val maftirAndHaftarah: Option[MaftirAndHaftarah]
  ):
    private def setMaftirAndHaftarah(maftirAndHaftarah: MaftirAndHaftarah): ReadingCustom = ReadingCustom(
      torah = torah,
      maftirAndHaftarah = Some(maftirAndHaftarah)
    )

    private def setHaftarah(haftarah: Haftarah): ReadingCustom =
      setMaftirAndHaftarah(MaftirAndHaftarah(maftirAndHaftarah.get.maftir, haftarah))

    def addHaftarah(haftaraAddition: Option[Haftarah]): ReadingCustom =
      haftaraAddition.fold(this)(haftaraAddition => addHaftarah(haftaraAddition))

    def addHaftarah(haftaraAddition: Haftarah): ReadingCustom =
      setHaftarah(maftirAndHaftarah.get.haftarah ++ haftaraAddition)

    def replaceHaftarah(haftarah: Haftarah): ReadingCustom =
      setHaftarah(haftarah)

    def replaceMaftirAndHaftarah(maftir: Maftir, haftarah: Haftarah): ReadingCustom =
      setMaftirAndHaftarah(MaftirAndHaftarah(Some(maftir), haftarah))

  def apply(torah: Torah): Reading = apply(Custom.Of(torah))

  def apply(torah: Torah.Customs): Reading =
    new Reading(mapValues(torah.customs)(torah => ReadingCustom(torah, maftirAndHaftarah = None)))

  def apply(torah: Torah, maftir: Option[Maftir], haftarah: Haftarah.Customs): Reading =
    apply(Custom.Of(torah), maftir = maftir, haftarah = haftarah)

  def apply(torah: Torah.Customs, maftir: Option[Maftir], haftarah: Haftarah.Customs): Reading = new Reading(
    torah.liftLR[Haftarah, ReadingCustom](haftarah, (_: Custom, torah: Torah, haftarah: Haftarah) =>
      ReadingCustom(torah, maftirAndHaftarah = Some(MaftirAndHaftarah(maftir, haftarah)))
    ).customs
  )
