package org.opentorah.texts.tanach

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * An `<annotation>` records what is known about a custom's reading without
 * giving it one. The point is that it must not change what is read: these are
 * the three weeks where Chabad follows Sefard and hamichlol, whose rite key
 * nests Chabad under Ashkenaz, would imply otherwise.
 */
final class InheritedAnnotationTest extends AnyFlatSpec, Matchers:

  private val weeks = Seq(Parsha.Shemini, Parsha.Masei, Parsha.Vayeilech)

  "an annotation" should "record the source for a custom that has no reading of its own" in:
    for parsha <- weeks do withClue(s"$parsha: "):
      ReadingSources.forParsha(parsha, Custom.Chabad).map(_.key) should contain ("chitas")
      ReadingSources.commentFor(parsha, Custom.Chabad).get should include ("Chitas")

  it should "leave the custom inheriting, not give it an entry" in:
    for parsha <- weeks do withClue(s"$parsha: "):
      parsha.haftarah.customs.keySet should not contain Custom.Chabad
      parsha.haftarah.doFind(Custom.Chabad) shouldBe parsha.haftarah.doFind(Custom.Sefard)

  it should "not disturb the other customs of those weeks" in:
    for parsha <- weeks do withClue(s"$parsha: "):
      parsha.haftarah.doFind(Custom.Ashkenaz) should not be parsha.haftarah.doFind(Custom.Sefard)
      ReadingSources.forParsha(parsha, Custom.Sefard).map(_.key) should not contain "chitas"

  "every custom" should "still resolve everywhere" in:
    for
      parsha <- Parsha.valuesSeq
      custom <- Custom.all
    do withClue(s"$parsha/$custom: ")(parsha.haftarah.find(custom) should not be empty)
