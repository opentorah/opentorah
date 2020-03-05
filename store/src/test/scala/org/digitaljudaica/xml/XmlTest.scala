package org.digitaljudaica.xml

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class XmlTest extends AnyFlatSpec with Matchers {

  "text parsing" should "work" in {
    Parser.parseOrError(From.xml(<s><a>asdjkh</a></s>).parse(
      Element("a", ContentType.Elements, Xml.text.optional).required)).isLeft shouldBe true
    Parser.parseDo(From.xml(<s><a>asdjkh</a></s>).parse(
      Element("a", ContentType.Text, Xml.text.required).required)) shouldBe "asdjkh"
  }

  "From.resource()" should "work" in {
    Parser.run(From.resource(Parser, "1").load)
  }

  private val file2parser: Parser[String] =
    Xml.withName("x", Element("name", ContentType.Text, Xml.text.required).required)

  "Include" should "work" in {
    Parser.parseDo(From.resource(Parser, "2").parse(
      file2parser)) shouldBe "X"
    Parser.parseDo(From.resource(Parser, "1").parse(
      Xml.withInclude("to", ContentType.Elements, file2parser))) shouldBe "X"
  }
}
