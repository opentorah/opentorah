package org.opentorah.xml

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class XmlTest extends AnyFlatSpec with Matchers {

  def parseOrError[A](parser: Parser[A]): Either[Error, A] =
    Parser.run(Parser.runnable(parser).either)

  "text parsing" should "work" in {
    parseOrError(
      new Element[Option[String]]("a") {
        override protected def contentType: ContentType = ContentType.Elements
        override protected def parser: Parser[Option[String]] = Text().optional
      }
        .parse(From.xml("test", <s>
          <a>asdjkh</a>
        </s>))
    ).isLeft shouldBe true

    Parser.parseDo(
      new Element[String]("a") {
        override protected def contentType: ContentType = ContentType.Characters
        override protected def parser: Parser[String] = Text().required
      }.parse(From.xml("test", <a>asdjkh</a>))
    ) shouldBe "asdjkh"
  }

  "From.resource()" should "work" in {
    Parser.run(From.resource(Parser, "1").load)
  }

  private val file2parsable: Parsable[String] =
    new Element[String]("x") {
      override protected def contentType: ContentType = ContentType.Elements
      override protected def parser: Parser[String] = new Element[String]("name") {
        override protected def contentType: ContentType = ContentType.Characters
        override protected def parser: Parser[String] = Text().required
      }.required
    }

  "Include" should "work" in {
    Parser.parseDo(file2parsable
      .parse(From.resource(Parser, "2"))) shouldBe "X"

    Parser.parseDo(Parsable.withInclude(file2parsable, "to")
      .parse(From.resource(Parser, "1"))) shouldBe "X"
  }
}
