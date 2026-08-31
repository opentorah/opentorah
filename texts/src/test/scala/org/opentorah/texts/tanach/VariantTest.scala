package org.opentorah.texts.tanach

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A `variant` records a reading the sources report without settling who
 * follows it. It must stay out of the way of resolution: nothing may read one
 * by accident, and every custom must still resolve to exactly one haftarah.
 */
final class VariantTest extends AnyFlatSpec, Matchers:

  private def joelStart(haftarah: Haftarah): Int =
    haftarah.spans.find(_.book == Tanach.Book.Joel).get.span.from.verse

  "Vayeilech Ashkenaz" should "read Joel from 2:15" in:
    joelStart(Parsha.Vayeilech.haftarah.doFind(Custom.Ashkenaz)) shouldBe 15

  it should "record the 2:11 opening as a variant, not as the reading" in:
    val variants = ReadingSources.variantsFor(Parsha.Vayeilech, Custom.Ashkenaz)
    variants.map(_.number) shouldBe Seq(2)
    joelStart(variants.head.haftarah) shouldBe 11
    variants.head.annotation.sources shouldBe Seq("steinsaltz")
    variants.head.annotation.comment.get should include ("some Ashkenazim")

  it should "belong to Ashkenaz alone, not to the customs under it" in:
    for custom <- Custom.all.filter(_.isUnder(Custom.Ashkenaz)).filterNot(_ == Custom.Ashkenaz) do
      withClue(s"$custom: ")(ReadingSources.variantsFor(Parsha.Vayeilech, custom) shouldBe empty)

  "a variant" should "not stop every custom resolving to one reading" in:
    for
      parsha <- Parsha.valuesSeq
      custom <- Custom.all
    do withClue(s"$parsha/$custom: ")(parsha.haftarah.find(custom) should not be empty)

  "every source an entry names" should "be one ReadingSources knows" in:
    // entries refer to sources by name now, so a typo is a runtime error
    // rather than a compile one; this is what catches it
    for
      (parsha, annotations) <- Haftarah.annotationsByParsha
      (custom, annotation) <- annotations
      key <- annotation.sources
    do withClue(s"$parsha/$custom: ")(ReadingSources.sources.keySet should contain (key))
