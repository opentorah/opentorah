package org.opentorah.xml

import org.opentorah.util.Effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.net.URL

final class XmlTest extends AnyFlatSpec, Matchers:

  def unsafeRun[A](parser: Parser[A]): A = Parser.unsafeRun(parser)

  def loadResource(xml: Xml, name: String): xml.Element =
    xml.loadFromUrl(Parsing.getClass.getResource(name + ".xml"))

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

  "From.resource()" should "work" in {
    loadResource(ScalaXml, "1")
    loadResource(Dom     , "1")
  }

  "My enhancements to scala-xml" should "be present" in {
    def roundtrip(xml: String): Unit = scala.xml.XML.loadString(xml).toString shouldBe xml

    roundtrip("<a><!-- comment --> suffix</a>")
    roundtrip("<a>prefix <!-- comment -->   <!-- comment2 --> suffix</a>")
    roundtrip("<a>prefix <b><!-- comment --></b> suffix</a>")
    roundtrip("<a>prefix <b><!-- multi-\nline\n comment --></b> suffix</a>")
    roundtrip("""<a>prefix <b><!-- multi-
                |line
                | comment --></b> suffix</a>""".stripMargin)

    roundtrip("<a><![CDATA[ cdata ]]> suffix</a>")
    roundtrip("<a>prefix <![CDATA[ cdata ]]> suffix</a>")
    roundtrip("<a>prefix <b><![CDATA[ cdata section]]></b> suffix</a>")
    roundtrip("""<a>prefix <b><![CDATA[
                | multi-
                | line    cdata
                |    section]]>   </b> suffix</a>""".stripMargin)

    // confirm that processing instructions were always processed correctly
    roundtrip("<a><?target content ?> suffix</a>")
    roundtrip("<a>prefix <?target content ?> suffix</a>")
    roundtrip("<a>prefix <b><?target content?> </b> suffix</a>")

    def roundtripNodes(xml: String): Unit = scala.xml.XML.loadStringNodes(xml).map(_.toString).mkString("") shouldBe xml
    roundtripNodes("<!-- prolog --><a>text</a>")
    roundtripNodes("<!-- prolog --><?target content ?><!-- comment2 --><a>text</a>")
    roundtripNodes("""<!-- prolog
                     |    --><?target content ?><!--
                     |  comment2 --><a>text</a>""".stripMargin)

    roundtripNodes("<a>text</a><!-- epilogue -->")
    roundtripNodes("<a>text</a><!-- epilogue --><?target content ?><!-- comment2 -->")

    // Note: at least with the JDK's Xerces, whitespace in the prolog and epilogue gets lost in parsing:
    // the parser does not fire any white-space related events, so:
    // does not work: roundtripNodes("<!-- c -->  <a/>")
    // does not work: roundtripNodes("<a/> <!-- epilogue -->")

    def roundtripNS(namespaceAware: Boolean, xml: String): Unit = {
      val parserFactory: javax.xml.parsers.SAXParserFactory = javax.xml.parsers.SAXParserFactory.newInstance()
      parserFactory.setNamespaceAware(namespaceAware)
      parserFactory.setXIncludeAware(namespaceAware)

      scala.xml.XML.withSAXParser(parserFactory.newSAXParser()).loadString(xml).toString() shouldBe xml
    }

    roundtripNS(namespaceAware = false, """<book xmlns="http://docbook.org/ns/docbook" xmlns:xi="http://www.w3.org/2001/XInclude"/>""")
    roundtripNS(namespaceAware = true , """<book xmlns="http://docbook.org/ns/docbook" xmlns:xi="http://www.w3.org/2001/XInclude"/>""")
    roundtripNS(namespaceAware = true , """<book xmlns="http://docbook.org/ns/docbook" xmlns:xi="http://www.w3.org/2001/XInclude"><svg xmlns:svg="http://www.w3.org/2000/svg"/></book>""")
  }

  "My enhancements to scala-xml" should "be usable" in {
    val result: ScalaXml.Element = loadResource(ScalaXml, "comments")
    result.toString.contains("<!-- Test Comment -->") shouldBe true
    // TODO test comments in prolog/epilog
    // TODO test namespace-aware
  }

  private final class X(
    val fromUrl: Element.FromUrl,
    val name: String
  )

  private val nameParsable: Element[String] = new Element[String]("name"):
    override def contentType: Element.ContentType = Element.ContentType.Characters

    override def contentParsable: Parsable[String] = new Parsable[String]:
      override def parser: Parser[String] = Text().required()
      override def unparser: Unparser[String] = ???

  private val file2element: Element[X] = new Element[X]("x"):
    override def contentType: Element.ContentType = Element.ContentType.Elements

    override def contentParsable: Parsable[X] = new Parsable[X]:
      override def parser: Parser[X] = for
        fromUrl <- Parsing.fromUrl
        name <- nameParsable.required()
      yield X(
        fromUrl,
        name
      )

      override def unparser: Unparser[X] = ???

  "Redirect" should "work" in {
    def resource(name: String) = From.resourceNamed(Parsing, name, ScalaXml) // TODO duplicate with Dom
    val r1 = resource("1")
    def checkUrl(url: URL, name: String): Unit = url.toString.endsWith(s"/$name.xml") shouldBe true

    val direct: X = unsafeRun(file2element.parse(resource("9")))
    direct.name shouldBe "X"
    checkUrl(direct.fromUrl.url, "9")

    val followed: X = unsafeRun(file2element.followRedirects.parse(r1))
    followed.name shouldBe "X"
    checkUrl(direct.fromUrl.url, "9")

    val redirect = unsafeRun(file2element.orRedirect.parse(r1)).swap.toOption.get
    checkUrl(redirect.from.url.get, "2")

    val redirected: X = unsafeRun(redirect.followRedirects)
    redirected.name shouldBe "X"
    checkUrl(direct.fromUrl.url, "9")

    val withTrue: X = unsafeRun(file2element.withRedirect(true).parse(r1)).toOption.get
    withTrue.name shouldBe "X"
    checkUrl(direct.fromUrl.url, "9")

    val withFalse = unsafeRun(file2element.withRedirect(false).parse(r1)).swap.toOption.get
    checkUrl(withFalse.from.url.get, "2")
  }

  "Attribute.get()" should "work" in {
    Attribute("id").optional.get(ScalaXml)(<x id="2"/>) shouldBe Some("2")
    Attribute("id", Xml.namespace).optional.get(ScalaXml)(<x xml:id="3"/>) shouldBe Some("3")
    Dom.getAttributes(loadResource(Dom, "namespace")) shouldBe empty
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
