package org.opentorah.metadata

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class MetadataTest extends AnyFlatSpec, Matchers:

  "Language" should "work" in {
    Language.English.names.names.length shouldBe 4
  }

  "Hebrew.numberToString" should "work" in {
    def check(number: Int, string: String): Unit =
      Language.Hebrew.numberToString(number) shouldBe string
      Language.Hebrew.numberFromString(string) shouldBe Some(number)

    check(  0, "")
    check(  5, "ה")
    check( 10, "י")
    check( 15, "טו")
    check( 20, "כ")
    check(100, "ק")
    check(116, "קטז")
    check(119, "קיט")
    check(555, "תקנה")
    check(999, "תתקצט")
    check(6000, "ו׳")

    def fail(string: String): Unit =
      Language.Hebrew.numberFromString(string) shouldBe None

    fail("הה")
    fail("ק׳")
  }
