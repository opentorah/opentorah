package org.opentorah.texts

import org.podval.metadata.Language
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class SelectorSpec extends AnyFlatSpec, Matchers:
  "Terms" should "load lesson, parsha, and verse" in:
    Terms.catalog.forName("lesson").get.names.hasName("урок") shouldBe true
    Terms.catalog.forName("parsha").get.names.hasName("parsha") shouldBe true
    Terms.catalog.forName("parsha").get.names.hasName("парша") shouldBe true
    Terms.catalog.forName("parsha").get.names.hasName("פרשה") shouldBe true
    Terms.catalog.forName("verse").get.names.hasName("стих") shouldBe true
    Terms.catalog.forName("книга").isDefined shouldBe true
    Terms.catalog.forName("ספר").isDefined shouldBe true
    Terms.catalog.forName("item").isEmpty shouldBe true

  it should "have a single day selector" in:
    Terms.catalog.terms.filter(_.selector).count(_.names.hasName("day")) shouldBe 1

  it should "have English, Russian, and Hebrew names on every selector" in:
    Terms.catalog.terms.filter(_.selector).foreach: term =>
      val langs = term.names.names.flatMap(_.languageSpec.language).toSet
      langs should contain allOf (Language.English, Language.Russian, Language.Hebrew)
