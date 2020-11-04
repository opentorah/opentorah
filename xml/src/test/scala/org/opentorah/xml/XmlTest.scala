package org.opentorah.xml

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.xml.sax.InputSource
import scala.xml.Elem

final class XmlTest extends AnyFlatSpec with Matchers {

  def parseOrError[A](parser: Parser[A]): Either[Error, A] =
    Parser.run(Parser.runnable(parser).either)

  def loadResource(name: String): Elem = Parser.load(From.resource(Parser, name))

  def parseResource(name: String): org.w3c.dom.Element =
    Saxon.Saxon10.parse(new InputSource(Parser.getClass.getResourceAsStream(name + ".xml")))
      .asInstanceOf[org.w3c.dom.Document].getDocumentElement

  def firstElement(element: Elem): Elem =
    Xml.getChildren(element).filter(Xml.isElement).map(Xml.asElement).head

  "text parsing" should "work" in {
    parseOrError(
      new Element[Option[String]]("a") {
        override protected def contentType: ContentType = ContentType.Elements
        override protected def parser: Parser[Option[String]] = Text().optional
      }
        .parse(From.xml("test", <s>
          <a>asdjkh</a>
        </s>))
    ).isLeft shouldBe true

    Parser.parseDo(
      new Element[String]("a") {
        override protected def contentType: ContentType = ContentType.Characters
        override protected def parser: Parser[String] = Text().required
      }.parse(From.xml("test", <a>asdjkh</a>))
    ) shouldBe "asdjkh"
  }

  "From.resource()" should "work" in {
    loadResource("1")
  }

  private val file2parsable: Parsable[String] =
    new Element[String]("x") {
      override protected def contentType: ContentType = ContentType.Elements
      override protected def parser: Parser[String] = new Element[String]("name") {
        override protected def contentType: ContentType = ContentType.Characters
        override protected def parser: Parser[String] = Text().required
      }.required
    }

  "Include" should "work" in {
    Parser.parseDo(file2parsable
      .parse(From.resource(Parser, "2"))) shouldBe "X"

    Parser.parseDo(Parsable.withInclude(file2parsable, "to")
      .parse(From.resource(Parser, "1"))) shouldBe "X"
  }

  "Attribute.get()" should "work" in {
    Attribute("id").get(<x id="2"/>) shouldBe Some("2")
    Attribute("id", Xml.namespace).get(<x xml:id="3"/>) shouldBe Some("3")
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
