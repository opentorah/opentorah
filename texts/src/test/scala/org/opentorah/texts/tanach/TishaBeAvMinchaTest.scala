package org.opentorah.texts.tanach

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Tisha BeAv Mincha is its own occasion in hamichlol, with a table that does
 * not follow the one shared by the other public fasts. Before this it fell
 * through to that one, which left Sefard and Teiman with no haftarah at all.
 */
final class TishaBeAvMinchaTest extends AnyFlatSpec, Matchers:

  private val mincha = SpecialReadings.TishaBeAv.afternoon(Parsha.Bereishis)

  private def haftarahOf(custom: Custom): Option[String] =
    mincha.doFind(custom).maftirAndHaftarah.map(_.haftarah.spans.map(span =>
      s"${span.book} ${span.span.from.chapter}:${span.span.from.verse}-" +
      s"${span.span.to.chapter}:${span.span.to.verse}").mkString("; "))

  private val Dirshu = Some("Isaiah 55:6-56:8")
  private val Shuva  = Some("Hosea 14:2-14:10")
  private val ShuvaWithMicah = Some("Hosea 14:2-14:10; Micah 7:18-7:20")

  "Ashkenaz and everything under it" should "read Dirshu" in:
    for custom <- Custom.all.filter(_.isUnder(Custom.Ashkenaz)).filterNot(_.isUnder(Custom.Italki)) do
      withClue(s"$custom: ")(haftarahOf(custom) shouldBe Dirshu)

  "Chabad" should "read Dirshu too, though it sits under Sefard" in:
    // verified on chabad.org; hamichlol's rite key nests Chabad under Ashkenaz,
    // so its silence at this occasion says the same
    haftarahOf(Custom.Chabad) shouldBe Dirshu

  "Sefard and everything under it" should "read Shuva with the Micah ending" in:
    for
      custom <- Custom.all.filter(_.isUnder(Custom.Sefard))
      if !custom.isUnder(Custom.Teiman) && !custom.isUnder(Custom.Chabad)
    do withClue(s"$custom: ")(haftarahOf(custom) shouldBe ShuvaWithMicah)

  "Italki and Teiman" should "read Shuva alone" in:
    for custom <- Custom.all.filter(c => c.isUnder(Custom.Italki) || c.isUnder(Custom.Teiman)) do
      withClue(s"$custom: ")(haftarahOf(custom) shouldBe Shuva)

  "every custom" should "read something, unlike on the other public fasts" in:
    for custom <- Custom.all do withClue(s"$custom: ")(haftarahOf(custom) should not be None)

  "the other public fasts" should "keep their own table" in:
    def onTeves(custom: Custom): Option[String] =
      SpecialReadings.FastOfTeves.afternoon(Parsha.Bereishis)
        .doFind(custom).maftirAndHaftarah.map(_.haftarah.spans.map(span =>
          s"${span.book} ${span.span.from.chapter}:${span.span.from.verse}-" +
          s"${span.span.to.chapter}:${span.span.to.verse}").mkString("; "))
    onTeves(Custom.Sefard) shouldBe None       // most Sefardim read nothing
    onTeves(Custom.Teiman) shouldBe None
    onTeves(Custom.Algiers) shouldBe Dirshu    // the city, but only here
    haftarahOf(Custom.Algiers) shouldBe ShuvaWithMicah
