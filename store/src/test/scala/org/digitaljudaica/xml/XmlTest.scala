package org.digitaljudaica.xml

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class XmlTest extends AnyFlatSpec with Matchers {
  "fromResource()" should "work" in {
    Load.fromResourceDo(Parse, "1")
  }

  "parsing" should "work" in {
    val byParser: Parse.Parser[String] = for {
      _ <- Parse.checkName("by")
    } yield "xyz"

    val storeParser: Parse.Parser[Option[String]] = for {
      _ <- Parse.checkName("store")
      result <- Parse.optionalElement("by", byParser)
    } yield result

    Parse.parse(Load.fromResource(Parse, "1"), storeParser)
  }
}
