package org.opentorah.metadata

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class MetadataTest extends AnyFlatSpec with Matchers {

  "Language" should "work" in {
    Language.English.names.names.length shouldBe 4
  }
}
