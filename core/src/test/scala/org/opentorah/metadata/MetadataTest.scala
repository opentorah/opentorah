package org.opentorah.metadata

import org.podval.xml.{XmlParser, Xml as ZioXml}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class MetadataTest extends AnyFlatSpec, Matchers:

  "Language" should "work" in:
    Language.English.names.names.length shouldBe 4
    Language.Hebrew.names.hasName("he") shouldBe true
    Language.Hebrew.names.hasName("иврит") shouldBe true

  "Name codec" should "accept n or text but not both" in:
    def decode(xml: String) =
      Name.codec.decode(XmlParser.parseXml(xml).toOption.get)(using ZioXml)

    val fromN = decode("""<name lang="en" n="English"/>""").toOption.get
    fromN.name shouldBe "English"
    fromN.languageSpec.language shouldBe Some(Language.English)

    val fromText = decode("""<name lang="ru">русский</name>""").toOption.get
    fromText.name shouldBe "русский"
    fromText.languageSpec.language shouldBe Some(Language.Russian)

    decode("""<name n="x">y</name>""").isLeft shouldBe true
    decode("""<name lang="en"/>""").isLeft shouldBe true
    decode("""<name lang="en" n="" transliterated="yes"/>""").toOption.get.name shouldBe ""

    val encoded = Name.codec.encode(fromN)(using ZioXml)
    encoded.get("n") shouldBe Some("English")
    encoded.get("lang") shouldBe Some("en")

  "Hebrew.numberToString" should "work" in:
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

