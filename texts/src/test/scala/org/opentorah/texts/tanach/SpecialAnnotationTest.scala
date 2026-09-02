package org.opentorah.texts.tanach

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The special readings carry sources, comments and variants like the weekly
 * ones. They are keyed by the day and the reading name, not by SpecialDay:
 * five of the readings are shared between occasions.
 */
final class SpecialAnnotationTest extends AnyFlatSpec, Matchers:

  "Pesach day 1" should "give back hamichlol's reading as a variant" in:
    val variants = ReadingSources.variantsForSpecialReading("Pesach1", "haftarah", Custom.Ashkenaz)
    variants.map(_.number) shouldBe Seq(2)
    variants.head.annotation.sources shouldBe Seq("michlol")
    variants.head.annotation.comment.get should include ("bare piece 1")
    ReadingSources.forSpecialReading("Pesach1", "haftarah", Custom.Ashkenaz)
      .map(_.key) should contain ("steinsaltz")

  "the city of Algiers" should "give back the note that Asulin differs" in:
    val comment = ReadingSources.commentForSpecialReading(
      "Fast", "defaultAfternoonHaftarah", Custom.Algiers).get
    comment should include ("Asulin")
    comment should include ("Dirshu or Shuva")

  "Agadir" should "give back what it reads at Tisha BeAv" in:
    ReadingSources.commentForSpecialReading(
      "TishaBeAv", "afternoonHaftarah", Custom.Agadir).get should include ("without the")

  "a reading with nothing recorded" should "simply say so" in:
    ReadingSources.forSpecialReading("RoshHashanah1", "haftarah", Custom.Ashkenaz) shouldBe empty
    ReadingSources.commentForSpecialReading("RoshHashanah1", "haftarah", Custom.Ashkenaz) shouldBe None

  "an annotation" should "not leak onto the customs sharing its entry" in:
    // Algiers shares its reading with Ashkenaz, Chabad and Romania, so all four
    // carry that entry's own comment -- but only Algiers carries the note about
    // Asulin, which is the whole reason it is an <annotation> and not a comment
    def comment(custom: Custom): String =
      ReadingSources.commentForSpecialReading("Fast", "defaultAfternoonHaftarah", custom).get
    comment(Custom.Algiers) should include ("Asulin")
    for custom <- Seq(Custom.Ashkenaz, Custom.Chabad, Custom.Romania) do
      withClue(s"$custom: "):
        comment(custom) should include ("hamichlol keeps only the city")
        comment(custom) should not include "Asulin"
