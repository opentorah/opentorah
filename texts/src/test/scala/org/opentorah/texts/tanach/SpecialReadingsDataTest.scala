package org.opentorah.texts.tanach

import org.podval.xml.{XmlAst, XmlDecode, XmlParser, Xml as ZioXml}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The readings moved out of SpecialReadings.scala into SpecialReadings.xml, so
 * what the compiler used to guarantee -- that every reading named is a reading
 * that exists -- now has to be checked.
 */
final class SpecialReadingsDataTest extends AnyFlatSpec, Matchers:
  private given xmlAst: XmlAst[ZioXml.Element] = ZioXml

  private val root: ZioXml.Element = XmlParser.parseResource(
    SpecialReadings.getClass,
    "SpecialReadings.xml"
  ).fold(error => throw error, identity)

  private val days: Seq[ZioXml.Element] = XmlDecode.childrenNamed(root, "day")

  private val readings: Seq[(String, String, ZioXml.Element)] = for
    day <- days
    reading <- XmlDecode.childrenNamed(day, "reading")
  yield (XmlDecode.requireAttr(day, "n"), XmlDecode.requireAttr(reading, "n"), reading)

  "SpecialReadings.xml" should "hold every reading, and each of them once" in:
    val keys: Seq[(String, String)] = readings.map((day, name, _) => (day, name))
    keys.size shouldBe 60
    keys.distinct shouldBe keys

  it should "give every reading a day and a name" in:
    for (day, name, _) <- readings do
      withClue(s"'$day'/'$name': ")((day.nonEmpty && name.nonEmpty) shouldBe true)

  it should "wrap exactly one torah, maftir or haftarah in each reading" in:
    for (day, name, reading) <- readings do
      val elements = reading.getChildren.flatMap(_.asElement)
      withClue(s"$day/$name: ")(elements.map(_.localName) should have size 1)
      withClue(s"$day/$name: ")
        (Seq("torah", "maftir", "haftarah") should contain (elements.head.localName))

  "the readings" should "all still parse and resolve" in:
    // forcing SpecialReadings parses every one of them; before the move, a
    // reading that failed to parse took the whole object's initialiser with it
    SpecialReadings.YomKippur.shabbos(Parsha.Bereishis).customs should not be empty
