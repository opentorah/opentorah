package org.digitaljudaica.xml

import cats.implicits._
import org.digitaljudaica.metadata.Names
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class XmlTest extends AnyFlatSpec with Matchers {
  "fromResource()" should "work" in {
    From.FromResource(Load, "1").doLoad
  }

  "error reporting" should "work" in {
    Context.run(Check.error("ErRoR")).isLeft shouldBe true
    Context.run(for { _ <- Check.error("ErRoR") } yield ()).isLeft shouldBe true
    Load(From.FromXml("error test", <store></store>), Names.withDefaultParser(None)).isLeft shouldBe true
  }

  "parsing" should "work" in {
    val byParser: Parser[String] = Element.checkName("by", pure("xyz"))

    val storeParser: Parser[Option[String]] = Element.checkName("store", Element.optional("by", byParser))

    Load(From.FromResource(Load, "1"), storeParser)
  }
}
