package org.opentorah.texts.tanach

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A combined week follows its second parsha, except where the first claims
 * customs back with precedenceWhenCombined. Both exceptions live in Haftarah.xml.
 */
final class CombinedHaftarahTest extends AnyFlatSpec, Matchers:

  "a combined week" should "follow its second parsha" in:
    val reading = WeeklyReading(Parsha.Tazria, Some(Parsha.Metzora)).getMorningReading
    for custom <- Seq(Custom.Ashkenaz, Custom.Sefard, Custom.Chabad) do
      haftarahOf(reading, custom) shouldBe Parsha.Metzora.haftarah.doFind(custom)

  "Nitzavim-Vayeilech" should "keep Nitzavim's haftarah for every custom" in:
    val reading = WeeklyReading(Parsha.Nitzavim, Some(Parsha.Vayeilech)).getMorningReading
    for custom <- Custom.all do
      withClue(s"$custom: ")(haftarahOf(reading, custom) shouldBe Parsha.Nitzavim.haftarah.doFind(custom))

  "Acharei-Kedoshim" should "keep Acharei's haftarah for Chabad only" in:
    val reading = WeeklyReading(Parsha.Acharei, Some(Parsha.Kedoshim)).getMorningReading
    haftarahOf(reading, Custom.Chabad) shouldBe Parsha.Acharei.haftarah.doFind(Custom.Chabad)
    for custom <- Custom.all.filterNot(_.isUnder(Custom.Chabad)) do
      withClue(s"$custom: ")(haftarahOf(reading, custom) shouldBe Parsha.Kedoshim.haftarah.doFind(custom))

  it should "be Amos for Ashkenaz and for Chabad, by different routes" in:
    val reading = WeeklyReading(Parsha.Acharei, Some(Parsha.Kedoshim)).getMorningReading
    // Ashkenaz because Amos is Kedoshim's own reading and the combined week
    // follows the second parsha; Chabad because Acharei takes precedence for it.
    haftarahOf(reading, Custom.Ashkenaz).spans.head.book shouldBe Tanach.Book.Amos
    haftarahOf(reading, Custom.Chabad).spans.head.book shouldBe Tanach.Book.Amos
    haftarahOf(reading, Custom.Sefard).spans.head.book shouldBe Tanach.Book.Ezekiel

  private def haftarahOf(reading: Reading, custom: Custom): Haftarah =
    reading.doFind(custom).maftirAndHaftarah.get.haftarah
