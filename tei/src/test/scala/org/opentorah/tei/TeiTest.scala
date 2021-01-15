package org.opentorah.tei

import org.opentorah.xml.{From, Html, LinkResolver, Parser, Xml}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class TeiTest extends AnyFlatSpec with Matchers {

  "Parsing" should "work" in {
//    println(Tei.prettyPrinter.renderXml(Parser.load(From.resource(Tei, "905"))))
    val tei: Tei = Parser.parseDo(Tei.parse(From.resource(Tei, "905")))
//    println(Tei.prettyPrinter.renderXml(Tei.toXmlElement(tei)))
  }

  "Entity parsing" should "work" in {
    val result: Entity = Parser.parseDo(
      Entity.parse(From.resource(Tei, "Баал_Шем_Тов")))

    result.role shouldBe Some("jew")
    result.name shouldBe "Израиль из Мезбича"
  }

  private def tei2html(element: Xml.Element): Xml.Element = {
    //    println(Xhtml.prettyPrinter.render(element))
    val resolver = new LinkResolver {
      override def resolve(url: Seq[String]): Option[Html.a] = None
      override def findByRef(ref:  String): Option[Html.a] = None
      override def facs(pageId: String): Option[Html.a] = Some(Html.a(
        path = Seq("facsimiles"),
        part = Some(pageId),
        target = Some("facsViewer")
      ))
    }

    Tei.toHtml(resolver, element)
  }

  "905" should "work" in {
    val tei: Tei = Parser.parseDo(Tei.parse(From.resource(Tei, "905")))
    val html: Xml.Element = tei2html(Tei.xmlElement(tei))
    //println(Tei.prettyPrinter.render(html))
  }
}
