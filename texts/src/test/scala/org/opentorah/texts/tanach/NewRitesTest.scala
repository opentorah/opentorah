package org.opentorah.texts.tanach

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The readings hamichlol gives the rites added with it. A custom with no
 * reading of its own resolves to its parent's, so an entry that goes missing
 * does not fail -- it quietly answers Ashkenaz or Sefard instead. Hence this.
 */
final class NewRitesTest extends AnyFlatSpec, Matchers:

  private def spans(haftarah: Haftarah): String = haftarah.spans.map(span =>
    s"${span.book} ${span.span.from.chapter}:${span.span.from.verse}-" +
    s"${span.span.to.chapter}:${span.span.to.verse}").mkString("; ")

  private def weekly(parsha: Parsha, custom: Custom): String =
    spans(parsha.haftarah.doFind(custom))

  "Poznan" should "read what hamichlol gives it, in all thirteen weeks" in:
    val expected = Seq(
      Parsha.Bereishis  -> "Isaiah 42:5-42:21",
      Parsha.Vayeira    -> "KingsII 4:1-4:23",
      Parsha.Va_eira     -> "Ezekiel 29:1-29:21",
      Parsha.Yisro      -> "Isaiah 6:1-6:13",
      Parsha.KiSisa     -> "KingsI 18:20-18:39",
      Parsha.Pekudei    -> "KingsI 8:1-8:21",
      Parsha.Shemini    -> "SamuelII 6:1-6:19",
      Parsha.Acharei    -> "Ezekiel 22:2-22:16",
      Parsha.Behar      -> "Jeremiah 32:6-32:22",
      Parsha.Bechukosai -> "Ezekiel 34:1-34:15",
      Parsha.Chukas     -> "Judges 11:1-11:24",
      Parsha.Vayeilech  -> "Isaiah 55:6-56:8",
      Parsha.Haazinu    -> "Hosea 14:2-14:10; Joel 2:15-2:27"
    )
    for (parsha, reading) <- expected do
      withClue(s"$parsha: ")(weekly(parsha, Custom.Poznan) shouldBe reading)

  it should "differ from Ashkenaz in every one of them" in:
    for parsha <- Parsha.valuesSeq do
      val differs: Boolean = weekly(parsha, Custom.Poznan) != weekly(parsha, Custom.Ashkenaz)
      withClue(s"$parsha: ")(differs `shouldBe` Set[Parsha](
        Parsha.Bereishis, Parsha.Vayeira, Parsha.Va_eira, Parsha.Yisro, Parsha.KiSisa,
        Parsha.Pekudei, Parsha.Shemini, Parsha.Acharei, Parsha.Behar, Parsha.Bechukosai,
        Parsha.Chukas, Parsha.Vayeilech, Parsha.Haazinu
      ).contains(parsha))

  "Persia, Libya and Pure Sephardim" should "read Ezekiel on Shemos, not Jeremiah with Sefard" in:
    for custom <- Seq(Custom.Persia, Custom.Libya, Custom.PureSephardim) do
      withClue(s"$custom: ")(weekly(Parsha.Shemos, custom) shouldBe "Ezekiel 16:1-16:14")
    weekly(Parsha.Shemos, Custom.Sefard) shouldBe "Jeremiah 1:1-2:3"

  "Algiers" should "reach the same reading through Algeria without an entry of its own" in:
    weekly(Parsha.Shemos, Custom.Algiers) shouldBe "Ezekiel 16:1-16:14"
    weekly(Parsha.Shemos, Custom.Algiers) shouldBe weekly(Parsha.Shemos, Custom.Magreb)

  "the city of Algiers" should "read Dirshu on a fast, where the country does not" in:
    val afternoon = SpecialReadings.FastOfTeves.afternoon(Parsha.Bereishis)
    def haftarahOf(custom: Custom): Option[String] =
      afternoon.doFind(custom).maftirAndHaftarah.map(mh => spans(mh.haftarah))
    haftarahOf(Custom.Algiers) shouldBe Some("Isaiah 55:6-56:8")
    haftarahOf(Custom.Ashkenaz) shouldBe haftarahOf(Custom.Algiers)
    haftarahOf(Custom.Algeria) should not be haftarahOf(Custom.Algiers)
    // most Sefardim read no haftarah at Mincha at all
    haftarahOf(Custom.Sefard) shouldBe None

  "Morocco and Algeria" should "read Dirshu on the Fast of Gedalia, Hosea on the rest" in:
    def haftarahOn(fast: SpecialReadings.Fast, custom: Custom): Option[String] =
      fast.afternoon(Parsha.Bereishis).doFind(custom).maftirAndHaftarah.map(mh => spans(mh.haftarah))
    for custom <- Seq(Custom.Morocco, Custom.Algeria) do withClue(s"$custom: "):
      haftarahOn(SpecialReadings.FastOfGedalia, custom) shouldBe Some("Isaiah 55:6-56:8")
      haftarahOn(SpecialReadings.FastOfTeves, custom) shouldBe
        Some("Hosea 14:2-14:10; Micah 7:18-7:20")
    // the city keeps Dirshu on both
    haftarahOn(SpecialReadings.FastOfTeves, Custom.Algiers) shouldBe Some("Isaiah 55:6-56:8")
    haftarahOn(SpecialReadings.FastOfGedalia, Custom.Algiers) shouldBe Some("Isaiah 55:6-56:8")

  "the new rites" should "cite hamichlol" in:
    ReadingSources.forParsha(Parsha.Chukas, Custom.Poznan).map(_.key) should contain ("michlol")
    ReadingSources.forParsha(Parsha.Shemos, Custom.Persia).map(_.key) should contain ("michlol")
