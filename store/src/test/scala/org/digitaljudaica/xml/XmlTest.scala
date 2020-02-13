package org.digitaljudaica.xml

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class XmlTest extends AnyFlatSpec with Matchers {

  "error reporting" should "work" in {
    Context.parse(Parser.error("ErRoR")).isLeft shouldBe true
    Context.parse(for { _ <- Parser.error[Unit]("ErRoR") } yield ()).isLeft shouldBe true
  }

  "characters parsing" should "work" in {
    From.xml(<s><a>asdjkh</a></s>).parse(Element.required("a", Characters.optional)).isLeft shouldBe true
    From.xml(<s><a>asdjkh</a></s>).parseDo(Element.withCharacters.required("a", Characters.required)) shouldBe "asdjkh"
  }

  "From.resource()" should "work" in {
    From.resource(Parser, "1").loadDo
  }

  private val file2parser: Parser[String] =
    Element.withName("x", Element.withCharacters.required("name", Characters.required))

  "Include" should "work" in {
    From.resource(Parser, "2").parseDo(file2parser) shouldBe "X"
    From.resource(Parser, "1").parseDo(Element.withInclude("to", file2parser)) shouldBe "X"
  }
}
