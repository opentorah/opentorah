package org.opentorah.xml

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class XmlTest extends AnyFlatSpec with Matchers {

  //  def parseOrError[A](parser: Parser[A]): Either[Error, A] =
  //    run(runnable(parser).either)

  // TODO
//  "text parsing" should "work" in {
//    Parser.parseOrError(From.xml(<s><a>asdjkh</a></s>).parse(
//      Element("a", ContentType.Elements, Text().optional).required)).isLeft shouldBe true
//    Parser.parseDo(From.xml(<s><a>asdjkh</a></s>).parse(
//      Element("a", ContentType.Text, Text().required).required)) shouldBe "asdjkh"
//  }

  "From.resource()" should "work" in {
    Parser.run(From.resource(Parser, "1").load)
  }

  // TODO
//  private val file2parser: Parser[String] =
//    Element.withName("x", Element("name", ContentType.Text, Text().required).required)
//
//  "Include" should "work" in {
//    Parser.parseDo(From.resource(Parser, "2").parse(
//      file2parser)) shouldBe "X"
//    Parser.parseDo(From.resource(Parser, "1").parse(
//      Parser.withInclude("to", ContentType.Elements, file2parser))) shouldBe "X"
//  }
}
