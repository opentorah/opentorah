package org.digitaljudaica.xml

import cats.implicits._
import org.digitaljudaica.metadata.Names
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class XmlTest extends AnyFlatSpec with Matchers {
  "fromResource()" should "work" in {
    From.resource(Load, "1").loadDo
  }

  "error reporting" should "work" in {
    Context.run(Parser.error("ErRoR")).isLeft shouldBe true
    Context.run(for { _ <- Parser.error[Unit]("ErRoR") } yield ()).isLeft shouldBe true
    From.xml(<store></store>).parse(Names.withDefaultParser(None)).isLeft shouldBe true
  }

  "parsing" should "work" in {
    val byParser: Parser[String] = Parser.checkName("by", Parser.pure("xyz"))

    val storeParser: Parser[Option[String]] = Parser.checkName("store", Element.optional("by", byParser))

    From.resource(Load, "1").parse(storeParser)
  }
}
