package org.opentorah.texts.tanach

import org.podval.xml.{XmlAst, XmlParser, Xml as ZioXml}
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

  private val days: Seq[ZioXml.Element] = root.childrenNamed("day")

  private val readings: Seq[(String, SpecialReadings.Slot, ZioXml.Element)] = for
    day <- days
    element <- day.getChildren.flatMap(_.asElement)
    tag = element.getName.localName
    if Seq("torah", "maftir", "haftarah").contains(tag)
  yield (
    day.requireAttr("n"),
    SpecialReadings.Slot(
      tag,
      when = element.get("when").map(_.trim).filter(_.nonEmpty),
      role = element.get("role").map(_.trim).filter(_.nonEmpty),
      n = element.get("n").map(_.trim).filter(_.nonEmpty).map(_.toInt)
    ),
    element
  )

  "SpecialReadings.xml" should "hold every reading, and each of them once" in:
    val keys: Seq[(String, SpecialReadings.Slot)] = readings.map((day, slot, _) => (day, slot))
    keys.size shouldBe 60
    keys.distinct shouldBe keys

  it should "give every reading a day" in:
    for (day, slot, _) <- readings do
      withClue(s"'$day'/$slot: ")(day.nonEmpty shouldBe true)

  it should "use only torah, maftir or haftarah as children of day" in:
    for (day, slot, _) <- readings do
      withClue(s"$day/$slot: ")
        (Seq("torah", "maftir", "haftarah") should contain (slot.tag))

  "the readings" should "all still parse and resolve" in:
    // forcing SpecialReadings parses every one of them; before the move, a
    // reading that failed to parse took the whole object's initialiser with it
    SpecialReadings.YomKippur.shabbos(Parsha.Bereishis).customs should not be empty
