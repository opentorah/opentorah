package org.opentorah.texts.tanach

import org.opentorah.xml.{From, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The readings moved out of SpecialReadings.scala into SpecialReadings.xml, so
 * what the compiler used to guarantee -- that every reading named is a reading
 * that exists -- now has to be checked.
 */
final class SpecialReadingsDataTest extends AnyFlatSpec, Matchers:

  private val root = Parser.unsafeRun(From.resourceNamed(SpecialReadings, "SpecialReadings").load)

  private val readings: Seq[(String, String)] = for
    day <- (root \ "day").toSeq
    reading <- (day \ "reading").toSeq
  yield (day \@ "n", reading \@ "n")

  "SpecialReadings.xml" should "hold every reading, and each of them once" in:
    readings.size shouldBe 60
    readings.distinct shouldBe readings

  it should "give every reading a day and a name" in:
    for (day, name) <- readings do
      withClue(s"'$day'/'$name': ")((day.nonEmpty && name.nonEmpty) shouldBe true)

  it should "wrap exactly one torah, maftir or haftarah in each reading" in:
    for
      day <- (root \ "day").toSeq
      reading <- (day \ "reading").toSeq
    do
      val elements = reading.child.collect { case elem: scala.xml.Elem => elem }
      withClue(s"${day \@ "n"}/${reading \@ "n"}: ")
        (elements.map(_.label) should have size 1)
      withClue(s"${day \@ "n"}/${reading \@ "n"}: ")
        (Seq("torah", "maftir", "haftarah") should contain (elements.head.label))

  "the readings" should "all still parse and resolve" in:
    // forcing SpecialReadings parses every one of them; before the move, a
    // reading that failed to parse took the whole object's initialiser with it
    SpecialReadings.YomKippur.shabbos(Parsha.Bereishis).customs should not be empty
