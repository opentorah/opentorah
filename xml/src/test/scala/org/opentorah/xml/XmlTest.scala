package org.opentorah.xml

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.xml.sax.InputSource
import java.net.URL

final class XmlTest extends AnyFlatSpec with Matchers {

  def parseOrError[A](parser: Parser[A]): Either[Error, A] =
    Parser.run(Parser.runnable(parser).either)

  def loadResource(name: String): Xml.Element = Parser.run(From.resource(Parser, name).load)

  def parseResource(name: String): org.w3c.dom.Element =
    Saxon.Saxon10.parse(new InputSource(Parser.getClass.getResourceAsStream(name + ".xml")))
      .asInstanceOf[org.w3c.dom.Document].getDocumentElement

  def firstElement(element: Xml.Element): Xml.Element =
    Xml.getChildren(element).filter(Xml.isElement).map(Xml.asElement).head

  "text parsing" should "work" in {
    parseOrError(
      new Element[Option[String]]("a") {
        override def contentType: ContentType = ContentType.Elements

        override def contentParsable: Parsable[Option[String]] = new Parsable[Option[String]] {
          override def parser: Parser[Option[String]] = Text().optional()
          override def unparser: Unparser[Option[String]] = ???
        }
      }
        .parse(From.xml("test", <s>
          <a>asdjkh</a>
        </s>))
    ).isLeft shouldBe true

    Parser.parseDo(
      new Element[String]("a") {
        override def contentType: ContentType = ContentType.Characters

        override def contentParsable: Parsable[String] = new Parsable[String] {
          override def parser: Parser[String] = Text().required()
          override def unparser: Unparser[String] = ???
        }
      }.parse(From.xml("test", <a>asdjkh</a>))
    ) shouldBe "asdjkh"
  }

  "From.resource()" should "work" in {
    loadResource("1")
  }

  private final class X(
    val fromUrl: FromUrl,
    val name: String
  )

  private val nameParsable: Element[String] = new Element[String]("name") {
    override def contentType: ContentType = ContentType.Characters

    override def contentParsable: Parsable[String] = new Parsable[String] {
      override def parser: Parser[String] = Text().required()
      override def unparser: Unparser[String] = ???
    }
  }

  private val file2element: Element[X] = new Element[X]("x") {
    override def contentType: ContentType = ContentType.Elements

    override def contentParsable: Parsable[X] = new Parsable[X] {
      override def parser: Parser[X] = for {
        urls <- Context.currentFromUrl
        name <- nameParsable.required()
      } yield new X(
        urls,
        name
      )

      override def unparser: Unparser[X] = ???
    }
  }

  "Redirect" should "work" in {
    def resource(name: String) = From.resource(Parser, name)
    val r1 = resource("1")
    def checkUrl(url: URL, name: String): Unit = url.toString.endsWith(s"/$name.xml") shouldBe true

    val direct: X = Parser.parseDo(file2element.parse(resource("9")))
    direct.name shouldBe "X"
    checkUrl(direct.fromUrl.url, "9")

    val followed: X = Parser.parseDo(file2element.followRedirects.parse(r1))
    followed.name shouldBe "X"
    checkUrl(direct.fromUrl.url, "9")

    val redirect = Parser.parseDo(file2element.orRedirect.parse(r1)).swap.toOption.get
    checkUrl(redirect.url, "2")

    val redirected: X = Parser.parseDo(redirect.followRedirects)
    redirected.name shouldBe "X"
    checkUrl(direct.fromUrl.url, "9")

    val withTrue: X = Parser.parseDo(file2element.withRedirect(true).parse(r1)).toOption.get
    withTrue.name shouldBe "X"
    checkUrl(direct.fromUrl.url, "9")

    val withFalse = Parser.parseDo(file2element.withRedirect(false).parse(r1)).swap.toOption.get
    checkUrl(withFalse.url, "2")
  }

  "Attribute.get()" should "work" in {
    Attribute("id").optional.get(<x id="2"/>) shouldBe Some("2")
    Attribute("id", Xml.namespace).optional.get(<x xml:id="3"/>) shouldBe Some("3")
    Dom.getAttributes(parseResource("namespace")) shouldBe Seq.empty
  }

  private val teiNamespace: Namespace = Namespace(prefix = "tei", uri = "http://www.tei-c.org/ns/1.0")

  "Namespace.get()" should "work" in {
    Namespace.get(<TEI/>) shouldBe Namespace.No
    Namespace.get(<TEI xml:id="3"/>) shouldBe Namespace.No
    Namespace.get(teiNamespace.declare(<tei:TEI/>)) shouldBe teiNamespace
    Namespace.get(teiNamespace.declare(<TEI/>)) shouldBe Namespace.No
    Namespace.get(teiNamespace.default.declare(<TEI/>)) shouldBe teiNamespace.default
    Namespace.get(<TEI xmlns={teiNamespace.uri}/>) shouldBe teiNamespace.default

    Namespace.get(loadResource("namespace")) shouldBe teiNamespace.default
    Namespace.get(firstElement(loadResource("namespace"))) shouldBe teiNamespace.default

    Namespace.get(parseResource("namespace")) shouldBe teiNamespace.default
  }
}
