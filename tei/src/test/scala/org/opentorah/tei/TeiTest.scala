package org.opentorah.tei

import org.opentorah.entity.Entity
import org.opentorah.xml.{From, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class TeiTest extends AnyFlatSpec with Matchers {

  "Parsing" should "work" in {
    val result: Tei = Parser.parseDo(Tei.parse(From.resource(Tei, "905")))
  }

  "Entity parsing" should "work" in {
    val result: Entity = Parser.parseDo(
      Entity.parse(From.resource(Tei, "Баал_Шем_Тов")))

    result.role shouldBe Some("jew")
    result.name shouldBe "Израиль из Мезбича"
  }
}
