package org.digitaljudaica.xml

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class XmlTest extends AnyFlatSpec with Matchers {

  "error reporting" should "work" in {
    Context.parse(Parser.error("ErRoR")).isLeft shouldBe true
    Context.parse(for { _ <- Parser.error[Unit]("ErRoR") } yield ()).isLeft shouldBe true
  }

  "text parsing" should "work" in {
    From.xml(<s><a>asdjkh</a></s>).elements
      .parse(Xml.element.elements.required("a", Xml.text.optional)).isLeft shouldBe true
    From.xml(<s><a>asdjkh</a></s>).elements
      .parseDo(Xml.element.characters.required("a", Xml.text.required)) shouldBe "asdjkh"
  }

  "From.resource()" should "work" in {
    From.resource(Parser, "1").loadDo
  }

  private val file2parser: Parser[String] =
    Xml.withName("x", Xml.element.characters.required("name", Xml.text.required))

  "Include" should "work" in {
    From.resource(Parser, "2").elements.parseDo(file2parser) shouldBe "X"
    From.resource(Parser, "1").elements.parseDo(Xml.withInclude("to", file2parser)) shouldBe "X"
  }
}
