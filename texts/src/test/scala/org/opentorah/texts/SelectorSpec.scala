package org.opentorah.texts

import org.podval.metadata.Language
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class SelectorSpec extends AnyFlatSpec, Matchers:
  "texts Selectors" should "load lesson, parsha, and verse" in:
    Selectors.getForName("lesson").names.hasName("урок") shouldBe true
    Selectors.getForName("parsha").names.hasName("parsha") shouldBe true
    Selectors.getForName("parsha").names.hasName("парша") shouldBe true
    Selectors.getForName("parsha").names.hasName("פרשה") shouldBe true
    Selectors.getForName("verse").names.hasName("стих") shouldBe true
    Selectors.forName("книга").isDefined shouldBe true
    Selectors.forName("ספר").isDefined shouldBe true
    Selectors.forName("item").isEmpty shouldBe true

  it should "have a single day selector" in:
    Selectors.valuesSeq.count(_.names.hasName("day")) shouldBe 1

  it should "have English, Russian, and Hebrew names on every selector" in:
    Selectors.valuesSeq.foreach: selector =>
      val langs = selector.names.names.flatMap(_.languageSpec.language).toSet
      langs should contain allOf (Language.English, Language.Russian, Language.Hebrew)
