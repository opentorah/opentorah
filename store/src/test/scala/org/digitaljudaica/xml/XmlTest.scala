package org.digitaljudaica.xml

import cats.implicits._
import org.digitaljudaica.metadata.Names
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class XmlTest extends AnyFlatSpec with Matchers {
  "fromResource()" should "work" in {
    Load.fromResourceDo(Resource(Parse, "1"))
  }

  "error reporting" should "work" in {
    Context.run(Parse.error("ErRoR")).isLeft shouldBe true
    Context.run(for { _ <- Parse.error("ErRoR") } yield ()).isLeft shouldBe true
    Context.parse(Load.fromXml("error test", <store></store>), Names.withDefaultParser(None)).isLeft shouldBe true
  }

  "parsing" should "work" in {
    val byParser: Parser[String] = Parse.checkName("by", Parse.pure("xyz"))

    val storeParser: Parser[Option[String]] = Parse.checkName("store", Parse.optionalElement("by", byParser))

    Context.parse(Load.fromResource(Resource(Parse, "1")), storeParser)
  }
}
