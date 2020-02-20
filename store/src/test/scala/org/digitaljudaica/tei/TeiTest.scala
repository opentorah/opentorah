package org.digitaljudaica.tei

import cats.implicits._
import org.digitaljudaica.xml.From
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class TeiTest extends AnyFlatSpec with Matchers {

  "Parsing" should "work" in {
    val result: Tei = From.resource(Tei, "905").parseDo(Tei.parser)
  }
}
