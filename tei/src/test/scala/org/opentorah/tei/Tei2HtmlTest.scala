package org.opentorah.tei

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.opentorah.xml.{From, Namespace, Parser, Xhtml}
import scala.xml.Elem

final class Tei2HtmlTest extends AnyFlatSpec with Matchers {

  private def tei2html(element: Elem): Elem = {
//    println(Xhtml.prettyPrinter.render(element))
    val resolver = new TeiResolver {
      override def resolve(url:  String): Option[TeiResolver.Resolved] = None
      override def findByRef(ref:  String): Option[TeiResolver.Resolved] = None
      override def facs: TeiResolver.Resolved = new TeiResolver.Resolved(
        url = Seq("facsimiles"),
        role = Some("facsViewer")
      )
    }

    val result = Tei2Html.transform(resolver, element)
    println(Xhtml.prettyPrinter.render(result))
    result
  }

  "905" should "work" in {
    val tei: Tei = Parser.parseDo(Tei.parse(From.resource(Tei, "905")))
    tei2html(Tei.toXmlElement(tei))
  }
}
