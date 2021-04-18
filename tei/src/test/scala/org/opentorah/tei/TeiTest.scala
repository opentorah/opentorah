package org.opentorah.tei

import org.opentorah.html
import org.opentorah.xml.{From, Parser, Xml}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import zio.{UIO, URIO, ZLayer}

final class TeiTest extends AnyFlatSpec with Matchers {

  "Parsing" should "work" in {
//    println(Tei.prettyPrinter.renderXml(Parser.load(From.resource(Tei, "905"))))
    val tei: Tei = Parser.run(Tei.parse(From.resource(Tei, "905")))
//    println(Tei.prettyPrinter.renderXml(Tei.toXmlElement(tei)))
  }

  "Entity parsing" should "work" in {
    val result: Entity = Parser.run(
      Entity.parse(From.resource(Tei, "Баал_Шем_Тов")))

    result.role shouldBe Some("jew")
    result.name shouldBe "Израиль из Мезбича"
  }

  private def tei2html(element: Xml.Element): Xml.Element = {
    //    println(Xhtml.prettyPrinter.render(element))
    val resolver: LinksResolver = new LinksResolver {
      override def resolve(path: Seq[String]): UIO[Option[html.a]] = URIO.none
      override def findByRef(ref:  String): UIO[Option[html.a]] = URIO.none
      override def facs(pageId: String): UIO[Option[html.a]] = UIO.some(html.a(Seq("facsimiles"))
        .setFragment(pageId)
        .setTarget("facsViewer")
      )
    }

    Parser.run(Tei.toHtml(element).provideLayer(ZLayer.succeed(resolver)))
  }

  "905" should "work" in {
    val tei: Tei = Parser.run(Tei.parse(From.resource(Tei, "905")))
    val html: Xml.Element = tei2html(Tei.xmlElement(tei))
    //println(Tei.prettyPrinter.render(html))
  }
}
