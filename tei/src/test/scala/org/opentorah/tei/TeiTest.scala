package org.opentorah.tei

import org.opentorah.xml.{From, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class TeiTest extends AnyFlatSpec with Matchers {

  "Parsing" should "work" in {
//    println(Tei.prettyPrinter.renderXml(Parser.load(From.resource(Tei, "905"))))
    val tei: Tei = Parser.parseDo(Tei.parse(From.resource(Tei, "905")))
//    println(Tei.prettyPrinter.renderXml(Tei.toXmlElement(tei)))
  }

  "Entity parsing" should "work" in {
    val result: Entity = Parser.parseDo(
      Entity.parse(From.resource(Tei, "Баал_Шем_Тов")))

    result.role shouldBe Some("jew")
    result.name shouldBe "Израиль из Мезбича"
  }
}
