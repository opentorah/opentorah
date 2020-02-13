package org.digitaljudaica.metadata

import org.digitaljudaica.xml.From
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class MetadataTest extends AnyFlatSpec with Matchers {

  "Names parsing and Language" should "work" in {
    From.xml(<store></store>).parse(Names.withDefaultParser(None)).isLeft shouldBe true
    Language.English.names.names.length shouldBe 4
  }
}
