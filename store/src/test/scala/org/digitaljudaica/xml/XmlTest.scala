package org.digitaljudaica.xml

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class XmlTest extends AnyFlatSpec with Matchers {

  "text parsing" should "work" in {
    From.xml(<s><a>asdjkh</a></s>).parseOrError(
      Xml.required("a", ContentType.Elements, Xml.text.optional)).isLeft shouldBe true
    From.xml(<s><a>asdjkh</a></s>).parseDo(
      Xml.required("a", ContentType.Text, Xml.text.required)) shouldBe "asdjkh"
  }

  "From.resource()" should "work" in {
    From.resource(Parser, "1").loadDo
  }

  private val file2parser: Parser[String] =
    Xml.withName("x", Xml.required("name", ContentType.Text, Xml.text.required))

  "Include" should "work" in {
    From.resource(Parser, "2").parseDo(file2parser) shouldBe "X"
    From.resource(Parser, "1").parseDo(Xml.withInclude("to", ContentType.Elements, file2parser)) shouldBe "X"
  }
}
