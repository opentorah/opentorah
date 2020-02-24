package org.digitaljudaica.metadata

import org.digitaljudaica.xml.{ContentType, From, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class MetadataTest extends AnyFlatSpec with Matchers {

  "Names parsing and Language" should "work" in {
    Parser.parseOrError(From.xml(<store></store>).parse(ContentType.Elements, Names.parserWithDefaultName(None))).isLeft shouldBe true
    Language.English.names.names.length shouldBe 4
  }
}
