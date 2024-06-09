package org.opentorah.tei

import org.opentorah.xml.{A, Dialect, Element, Namespace, Parsable, Parser, PrettyPrinter, Unparser}

final class Tei(
  val teiHeader: TeiHeader,
  val text: Text
):
  def copy(
    teiHeader: TeiHeader = teiHeader,
    text: Text = text
  ): Tei = Tei(
    teiHeader,
    text
  )

object Tei extends Element[Tei]("TEI"), Dialect:

  override val namespace: Namespace = Namespace(uri = "http://www.tei-c.org/ns/1.0", prefix="tei")

  override val mimeType: String = "application/tei+xml"

  override val rootElementName: String = "TEI"

  override val dtdId: Option[String] = Some("-//TEI P5")

  /////  """<?xml-model href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng" schematypens="http://relaxng.org/ns/structure/1.0"?>\n"""
  override val prettyPrinter: PrettyPrinter = PrettyPrinter(
    doNotStackElements = Set("choice"),
    nestElements = Set("p", /*"abstract",*/ "head", "salute", "dateline"),
    clingyElements = Set("note", "lb", "sic", "corr")
  )

  override def contentParsable: Parsable[Tei] = new Parsable[Tei]:
    override lazy val parser: Parser[Tei] = for
      teiHeader: TeiHeader <- TeiHeader.required()
      text: Text <- Text.required()
    yield Tei(
      teiHeader,
      text
    )

    override lazy val unparser: Unparser[Tei] = concat(
      TeiHeader.required(_.teiHeader),
      Text.required(_.text)
    )

  def concat[A](unparsers: Unparser[A]*): Unparser[A] =
    Unparser.concatInNamespace(Tei.namespace, unparsers)
