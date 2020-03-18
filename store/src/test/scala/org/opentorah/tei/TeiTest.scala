package org.opentorah.tei

import org.opentorah.reference.Named
import org.opentorah.xml.{From, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class TeiTest extends AnyFlatSpec with Matchers {

  "Parsing" should "work" in {
    val result: Tei = Parser.parseDo(Tei.parse(From.resource(Tei, "905")))
  }

  "Named parsing" should "work" in {
    val result: Named = Parser.parseDo(
      Named.parse(From.resource(Tei, "Баал_Шем_Тов")))

    result.role shouldBe Some("jew")
    result.name shouldBe "Израиль из Мезбича"
  }
}
