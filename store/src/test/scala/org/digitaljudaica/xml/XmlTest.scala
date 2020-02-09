package org.digitaljudaica.xml

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class XmlTest extends AnyFlatSpec with Matchers {
  "loadResource()" should "work" in {
    Loader.doLoadResource(Xml, "1")
  }

  "parsing" should "work" in {
    val byParser: Xml.Parser[String] = for {
      _ <- Xml.checkName("by")
    } yield "xyz"

    val storeParser: Xml.Parser[Option[String]] = for {
      _ <- Xml.checkName("store")
      result <- Xml.optionalElement("by", byParser)
    } yield result

    Xml.parse(Loader.loadResource(Xml, "1"), storeParser)
  }
}
