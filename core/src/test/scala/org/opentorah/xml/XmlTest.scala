package org.opentorah.xml

import org.opentorah.util.Effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class XmlTest extends AnyFlatSpec, Matchers:

  def unsafeRun[A](parser: Parser[A]): A = Parser.unsafeRun(parser)

  def loadResource(name: String, fixXercesXIncludes: Boolean = true): Element =
    Effects.unsafeRun(From.resourceNamed(Xml, name, fixXercesXIncludes).load)

  private def loadSite(fixXercesXIncludes: Boolean) = loadResource("site/site", fixXercesXIncludes)

  "text parsing" should "work" in:
    unsafeRun(
      new ElementTo[String]("a") {
        override def contentType: ContentType = ContentType.Characters

        override def contentParsable: Parsable[String] = new Parsable[String]:
          override def parser: Parser[String] = Text().required()
          override def unparser: Unparser[String] = ???
      }.parse(From.xml("test", <a>asdjkh</a>))
    ) shouldBe "asdjkh"

  it should "fail the right way" in:
    unsafeRun((
      new ElementTo[Option[String]]("a") {
        override def contentType: ContentType = ContentType.Elements

        override def contentParsable: Parsable[Option[String]] = new Parsable[Option[String]]:
          override def parser: Parser[Option[String]] = Text().optional()
          override def unparser: Unparser[Option[String]] = ???
      }
        .parse(From.xml("test", <s><a>asdjkh</a></s>))
    ).either).isLeft shouldBe true

  "XInclude" should "work" in:
    loadResource("includer").toString shouldBe
      s"""|<includer>
          |  <includee xml:base="includee.xml">
          |  <content>Blah!</content>
          |</includee>
          |</includer>""".stripMargin

  it should "manifest a bug in Xerces" in:
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

  it should "fix a bug in Xerces with my fixer" in:
    loadSite(fixXercesXIncludes = true).toString shouldBe
      s"""|<site xmlns:xi="http://www.w3.org/2001/XInclude">
          |  <store xml:base="archive/books.xml" xmlns:xi="http://www.w3.org/2001/XInclude">
          |  <store xml:base="archive/books/book/derzhavin.xml" xmlns:xi="http://www.w3.org/2001/XInclude">
          |  <collection pageType="book" n="6" directory="6" xml:base="archive/books/book/derzhavin/volume/6.xml"/>
          |</store>
          |</store>
          |</site>""".stripMargin

  "Attribute.get()" should "work" in:
    unsafeRun(Attribute("id").optional.get(<x id="2"/>)) shouldBe Some("2")
    unsafeRun(Attribute("id", Xml.namespace).optional.get(<x xml:id="3"/>)) shouldBe Some("3")

  private val teiNamespace: Namespace = Namespace(prefix = "tei", uri = "http://www.tei-c.org/ns/1.0")

  "ScalaXml.getNamespace()" should "work" in:
    Namespace.get(<TEI/>) shouldBe Namespace.No
    Namespace.get(<TEI xml:id="3"/>) shouldBe Namespace.No
    Namespace.get(teiNamespace.declare(<tei:TEI/>)) shouldBe teiNamespace
    Namespace.get(teiNamespace.declare(<TEI/>)) shouldBe Namespace.No
    Namespace.get(teiNamespace.default.declare(<TEI/>)) shouldBe teiNamespace.default
    Namespace.get(<TEI xmlns={teiNamespace.uri}/>) shouldBe teiNamespace.default
    Namespace.get(<TEI xmlns="http://www.tei-c.org/ns/1.0"><teiHeader/></TEI>) shouldBe teiNamespace.default
    Namespace.get(loadResource("namespace")) shouldBe teiNamespace.default
    Namespace.get(firstElement(loadResource("namespace"))) shouldBe teiNamespace.default

  private def firstElement(element: Element): Element =
    Element.getChildren(element).filter(Element.is).map(Element.as).head
