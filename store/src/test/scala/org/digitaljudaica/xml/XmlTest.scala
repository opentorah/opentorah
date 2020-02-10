package org.digitaljudaica.xml

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class XmlTest extends AnyFlatSpec with Matchers {
  "fromResource()" should "work" in {
    Load.fromResourceDo(Resource(Parse, "1"))
  }

  "parsing" should "work" in {
    val byParser: Parse.Parser[String] = Parse.checkName("by", Parse.pure("xyz"))

    val storeParser: Parse.Parser[Option[String]] = Parse.checkName("store", Parse.optionalElement("by", byParser))

    Parse.parse(Load.fromResource(Resource(Parse, "1")), storeParser)
  }
}
