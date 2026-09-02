package org.opentorah.texts.tanach

import org.opentorah.xml.{From, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `<none>` and saying nothing are different things, and the difference is the
 * whole point: an entry whose value is None stops resolution, where no entry
 * at all continues up to the parent. Without that, a custom cannot read
 * nothing where its parent reads something.
 */
final class ReadsNothingTest extends AnyFlatSpec, Matchers:

  private val table: Haftarah.OptionalCustoms = Parser.unsafeRun(
    Haftarah.optionalElement(full = false).parse(From.xml("test",
      <haftarah>
        <custom n="Magreb" book="Hosea" fromChapter="14" fromVerse="2" toVerse="10"/>
        <none n="Morocco" comment="reads nothing, though Magreb above it reads Hosea"/>
      </haftarah>)))

  "a custom with <none>" should "read nothing rather than its parent's reading" in:
    table.find(Custom.Morocco) shouldBe Some(None)
    table.find(Custom.Magreb).flatten should not be None

  it should "pass that on to its own children" in:
    table.find(Custom.Fes) shouldBe Some(None)

  "a custom with no entry" should "still inherit" in:
    table.find(Custom.Djerba).flatten.map(_.spans.head.book) shouldBe Some(Tanach.Book.Hosea)

  "the two" should "be distinguishable in the map itself" in:
    // Morocco has an entry, whose value is None; Djerba has no entry at all
    table.customs.keySet should contain (Custom.Morocco)
    table.customs.keySet should not contain Custom.Djerba
    // and a custom with no entry anywhere above it resolves to nothing
    table.find(Custom.Teiman) shouldBe None
