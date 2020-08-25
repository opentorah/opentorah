package org.opentorah.tei

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.opentorah.xml.{From, Namespace, Parser, Xhtml}
import scala.xml.Elem

final class Tei2HtmlTest extends AnyFlatSpec with Matchers {

  private def tei2html(element: Elem): Elem = {
//    println(Xhtml.prettyPrinter.render(element))
    val result = Tei2Html.transformElement(element, Tei2Html.emptyEndNotes)._1
    println(Xhtml.prettyPrinter.render(result))
    result
  }

  "tei2html" should "work" in {
    val result = tei2html(
        <pb
        xmlns={Tei.namespace.uri}
        xml:id="p002-1"
        rendition="page"
        role="facsimileViewer"
        target="/collections/lvia1799-178/facs/002.html#p002-1"
        />
    )
    Namespace.get(result) shouldBe Xhtml.namespace.default
  }

  "905" should "work" in {
    val tei: Tei = Parser.parseDo(Tei.parse(From.resource(Tei, "905")))
    tei2html(Tei.toXmlElement(tei))
  }
}
