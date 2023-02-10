package org.opentorah.xml

import org.opentorah.util.Effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.net.URL

final class XmlTest extends AnyFlatSpec, Matchers:

  def unsafeRun[A](parser: Parser[A]): A = Parser.unsafeRun(parser)

  def loadResource(xml: Xml, name: String): xml.Element =
    Effects.unsafeRun(From.resourceNamed(Xml, name, xml).load.map(_.asInstanceOf[xml.Element]))

  "text parsing" should "work" in {
    unsafeRun(
      new Element[String]("a") {
        override def contentType: Element.ContentType = Element.ContentType.Characters

        override def contentParsable: Parsable[String] = new Parsable[String]:
          override def parser: Parser[String] = Text().required()
          override def unparser: Unparser[String] = ???
      }.parse(From.scalaXml("test", <a>asdjkh</a>))
    ) shouldBe "asdjkh"
  }

  it should "fail the right way" in {
    unsafeRun((
      new Element[Option[String]]("a") {
        override def contentType: Element.ContentType = Element.ContentType.Elements

        override def contentParsable: Parsable[Option[String]] = new Parsable[Option[String]]:
          override def parser: Parser[Option[String]] = Text().optional()
          override def unparser: Unparser[Option[String]] = ???
      }
        .parse(From.scalaXml("test", <s><a>asdjkh</a></s>))
    ).either).isLeft shouldBe true
  }

  "XInclude" should "work" in {
    loadResource(ScalaXml, "includer").toString shouldBe
      s"""|<includer>
          |  <includee xml:base="includee.xml">
          |  <content>Blah!</content>
          |</includee>
          |</includer>""".stripMargin
  }

  private def loadSite(fixXercesXIncludes: Boolean) = ScalaXml.load0(
    inputSource = Sax.url2inputSource(Parsing.getClass.getResource("site/site.xml")),
    filters = Seq.empty,
    resolver = None,
    processIncludes = Xerces.ProcessIncludes.YesWithBases,
    fixXercesXIncludes = fixXercesXIncludes
  )

  it should "manifest a bug in Xerces" in {
    // xml:base on the third level of includes is wrong:
    //           books/book/derzhavin/volume/6.xml instead of
    //   archive/books/book/derzhavin/volume/6.xml!
    loadSite(fixXercesXIncludes = false).toString shouldBe
      s"""|<site xmlns:xi="http://www.w3.org/2001/XInclude">
          |  <store xml:base="archive/books.xml" xmlns:xi="http://www.w3.org/2001/XInclude">
          |  <store xml:base="archive/books/book/derzhavin.xml" xmlns:xi="http://www.w3.org/2001/XInclude">
          |  <collection pageType="book" n="6" directory="6" xml:base="books/book/derzhavin/volume/6.xml"/>
          |</store>
          |</store>
          |</site>""".stripMargin
  }

  it should "fix a bug in Xerces with my fixer" in {
    loadSite(fixXercesXIncludes = true).toString shouldBe
      s"""|<site xmlns:xi="http://www.w3.org/2001/XInclude">
          |  <store xml:base="archive/books.xml" xmlns:xi="http://www.w3.org/2001/XInclude">
          |  <store xml:base="archive/books/book/derzhavin.xml" xmlns:xi="http://www.w3.org/2001/XInclude">
          |  <collection pageType="book" n="6" directory="6" xml:base="archive/books/book/derzhavin/volume/6.xml"/>
          |</store>
          |</store>
          |</site>""".stripMargin
  }

  "Attribute.get()" should "work" in {
    Attribute("id").optional.get(ScalaXml)(<x id="2"/>) shouldBe Some("2")
    Attribute("id", Xml.namespace).optional.get(ScalaXml)(<x xml:id="3"/>) shouldBe Some("3")
    Dom.getAttributes(loadResource(Dom, name = "namespace")) shouldBe empty
  }

  private val teiNamespace: Namespace = Namespace(prefix = "tei", uri = "http://www.tei-c.org/ns/1.0")

  "ScalaXml.getNamespace()" should "work" in {
    ScalaXml.getNamespace(<TEI/>) shouldBe Namespace.No
    ScalaXml.getNamespace(<TEI xml:id="3"/>) shouldBe Namespace.No
    ScalaXml.getNamespace(ScalaXml.declareNamespace(teiNamespace, <tei:TEI/>)) shouldBe teiNamespace
    ScalaXml.getNamespace(ScalaXml.declareNamespace(teiNamespace, <TEI/>)) shouldBe Namespace.No
    ScalaXml.getNamespace(ScalaXml.declareNamespace(teiNamespace.default, <TEI/>)) shouldBe teiNamespace.default
    ScalaXml.getNamespace(<TEI xmlns={teiNamespace.uri}/>) shouldBe teiNamespace.default

    ScalaXml.getNamespace(<TEI xmlns="http://www.tei-c.org/ns/1.0"><teiHeader/></TEI>) shouldBe teiNamespace.default

    ScalaXml.getNamespace(loadResource(ScalaXml, "namespace")) shouldBe teiNamespace.default
    ScalaXml.getNamespace(firstElement(loadResource(ScalaXml, "namespace"))) shouldBe teiNamespace.default

    Dom.getNamespace(loadResource(Dom, "namespace")) shouldBe teiNamespace.default
  }

  private def firstElement(element: ScalaXml.Element): ScalaXml.Element =
    ScalaXml.getChildren(element).filter(ScalaXml.isElement).map(ScalaXml.asElement).head
