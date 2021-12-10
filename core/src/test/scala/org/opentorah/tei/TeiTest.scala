package org.opentorah.tei

import org.opentorah.html.A
import org.opentorah.xml.{From, Parser, Parsing, ScalaXml}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import zio.{UIO, URIO, ZLayer}

final class TeiTest extends AnyFlatSpec, Matchers:

  def unsafeRun[A](parser: Parser[A]): A = Parser.unsafeRun(parser)

  def fromResource(name: String): From = From.resourceNamed(Tei, name, ScalaXml)

  def unsafeRun(name: String): Tei = unsafeRun(Tei.parse(fromResource(name)))

  "Parsing" should "work" in {
//    println(Tei.prettyPrinter.renderWithHeader(Parser.load(From.resource(Tei, "905"))))
    val tei: Tei = unsafeRun("905")
//    println(Tei.prettyPrinter.renderWithHeader(Tei.toXmlElement(tei)))
  }

  "Entity parsing" should "work" in {
    val result: Entity = unsafeRun(Entity.parse(fromResource("Баал_Шем_Тов")))

    result.role shouldBe Some("jew")
    result.name shouldBe "Израиль из Мезбича"
  }

  private def tei2html(element: ScalaXml.Element): ScalaXml.Element =
    //    println(Xhtml.prettyPrinter.render(element))
    val resolver: LinksResolver = new LinksResolver:
      override def resolve(path: Seq[String]): UIO[Option[A]] = URIO.none
      override def findByRef(ref:  String): UIO[Option[A]] = URIO.none
      override def facs(pageId: String): UIO[Option[A]] = UIO.some(A(Seq("facsimiles"))
        .setFragment(pageId)
        .setTarget("facsViewer")
      )

    unsafeRun(Tei.toHtml(element).provideLayer(ZLayer.succeed(resolver)))

  "905" should "work" in {
    val tei: Tei = unsafeRun("905")
    val html: ScalaXml.Element = tei2html(Tei.xmlElement(tei))
    //println(Tei.prettyPrinter.render(ScalaXml)(html))
  }
