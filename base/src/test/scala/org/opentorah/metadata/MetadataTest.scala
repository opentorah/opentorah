package org.opentorah.metadata

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class MetadataTest extends AnyFlatSpec, Matchers:

  "Language" should "work" in {
    Language.English.names.names.length shouldBe 4
  }
