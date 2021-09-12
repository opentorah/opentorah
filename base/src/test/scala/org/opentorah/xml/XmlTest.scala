package org.opentorah.xml

import org.opentorah.util.Effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.net.URL

final class XmlTest extends AnyFlatSpec, Matchers:

  def parseOrError[A](parser: Parser[A]): Either[Effects.Error, A] =
    Parser.unsafeRun(parser.either)

  def loadResource(xml: Xml, name: String): xml.Element =
    xml.loadFromUrl(Parser.getClass.getResource(name + ".xml"))

  def firstElement(element: ScalaXml.Element): ScalaXml.Element =
    ScalaXml.getChildren(element).filter(ScalaXml.isElement).map(ScalaXml.asElement).head

  "text parsing" should "work" in {
    parseOrError(
      new Element[Option[String]]("a") {
        override def contentType: ContentType = ContentType.Elements

        override def contentParsable: Parsable[Option[String]] = new Parsable[Option[String]]:
          override def parser: Parser[Option[String]] = Text().optional()
          override def unparser: Unparser[Option[String]] = ???
      }
        .parse(From.xml("test", <s>
          <a>asdjkh</a>
        </s>))
    ).isLeft shouldBe true

    Parser.unsafeRun(
      new Element[String]("a") {
        override def contentType: ContentType = ContentType.Characters

        override def contentParsable: Parsable[String] = new Parsable[String]:
          override def parser: Parser[String] = Text().required()
          override def unparser: Unparser[String] = ???
      }.parse(From.xml("test", <a>asdjkh</a>))
    ) shouldBe "asdjkh"
  }

  "From.resource()" should "work" in {
    loadResource(ScalaXml, "1")
  }

  private final class X(
    val fromUrl: FromUrl,
    val name: String
  )

  private val nameParsable: Element[String] = new Element[String]("name"):
    override def contentType: ContentType = ContentType.Characters

    override def contentParsable: Parsable[String] = new Parsable[String]:
      override def parser: Parser[String] = Text().required()
      override def unparser: Unparser[String] = ???

  private val file2element: Element[X] = new Element[X]("x"):
    override def contentType: ContentType = ContentType.Elements

    override def contentParsable: Parsable[X] = new Parsable[X]:
      override def parser: Parser[X] = for
        urls <- Context.currentFromUrl
        name <- nameParsable.required()
      yield X(
        urls,
        name
      )

      override def unparser: Unparser[X] = ???

  "Redirect" should "work" in {
    def resource(name: String) = From.resource(Parser, name)
    val r1 = resource("1")
    def checkUrl(url: URL, name: String): Unit = url.toString.endsWith(s"/$name.xml") shouldBe true

    val direct: X = Parser.unsafeRun(file2element.parse(resource("9")))
    direct.name shouldBe "X"
    checkUrl(direct.fromUrl.url, "9")

    val followed: X = Parser.unsafeRun(file2element.followRedirects.parse(r1))
    followed.name shouldBe "X"
    checkUrl(direct.fromUrl.url, "9")

    val redirect = Parser.unsafeRun(file2element.orRedirect.parse(r1)).swap.toOption.get
    checkUrl(redirect.url, "2")

    val redirected: X = Parser.unsafeRun(redirect.followRedirects)
    redirected.name shouldBe "X"
    checkUrl(direct.fromUrl.url, "9")

    val withTrue: X = Parser.unsafeRun(file2element.withRedirect(true).parse(r1)).toOption.get
    withTrue.name shouldBe "X"
    checkUrl(direct.fromUrl.url, "9")

    val withFalse = Parser.unsafeRun(file2element.withRedirect(false).parse(r1)).swap.toOption.get
    checkUrl(withFalse.url, "2")
  }

  "Attribute.get()" should "work" in {
    Attribute("id").optional.get(ScalaXml)(<x id="2"/>) shouldBe Some("2")
    Attribute("id", Xml.namespace).optional.get(ScalaXml)(<x xml:id="3"/>) shouldBe Some("3")
    Dom.getAttributes(loadResource(Dom, "namespace")) shouldBe Seq.empty
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
