package org.opentorah.tei

import org.opentorah.xml.{From, LinkResolver, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.xml.Elem

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

  private def tei2html(element: Elem): Elem = {
    //    println(Xhtml.prettyPrinter.render(element))
    val resolver = new LinkResolver {
      override def resolve(url: Seq[String]): Option[LinkResolver.Resolved] = None
      override def findByRef(ref:  String): Option[LinkResolver.Resolved] = None
      override def facs: LinkResolver.Resolved = LinkResolver.Resolved(
        url = Seq("facsimiles"),
        role = Some("facsViewer")
      )
    }

    Tei.toHtml(resolver, element)
  }

  "905" should "work" in {
    val tei: Tei = Parser.parseDo(Tei.parse(From.resource(Tei, "905")))
    val html: Elem = tei2html(Tei.toXmlElement(tei))
    println(Tei.prettyPrinter.render(html))
  }
}
