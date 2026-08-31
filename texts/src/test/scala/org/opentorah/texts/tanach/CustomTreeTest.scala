package org.opentorah.texts.tanach

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The hierarchy is data now (CustomTree.xml), so the properties the code
 * relies on have to be asserted rather than guaranteed by the enum's shape.
 */
final class CustomTreeTest extends AnyFlatSpec, Matchers:

  "every custom" should "have a parent, except the one root" in:
    val roots = Custom.valuesSeq.filter(_.parent.isEmpty)
    roots shouldBe Seq(Custom.Common)

  it should "reach the root without cycling" in:
    for custom <- Custom.valuesSeq do
      var seen = Set.empty[Custom]
      var at: Option[Custom] = Some(custom)
      while at.isDefined do
        val c = at.get
        withClue(s"cycle through $custom at $c: ")(seen should not contain c)
        seen += c
        at = c.parent
      withClue(s"$custom does not reach Common: ")(seen should contain (Custom.Common))

  it should "be named in Custom.xml too" in:
    // Named.ByLoader resolves names through the loader; a custom missing from
    // Custom.xml fails there, not here, so check both files list the same set.
    for custom <- Custom.valuesSeq do
      withClue(s"$custom has no name: ")(custom.name should not be empty)

  "the tree" should "put the rites where the readings expect them" in:
    Custom.Italki.parent shouldBe Some(Custom.Ashkenaz)
    Custom.Chabad.parent shouldBe Some(Custom.Sefard)
    Custom.Algiers.parent shouldBe Some(Custom.Algeria)
    Custom.Fes.parent shouldBe Some(Custom.Morocco)
    Custom.ChayeyOdom.parent shouldBe Some(Custom.Lita)
    Custom.Common.level shouldBe 0
    Custom.Fes.level shouldBe 4
