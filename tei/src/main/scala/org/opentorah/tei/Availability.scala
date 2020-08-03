package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, Element, Parser}
import scala.xml.Node

final case class Availability(
  status: Option[String],
  xml: Seq[Node]
)

object Availability extends Element.WithToXml[Availability]("availability") {

  private val statusAttribute: Attribute[String] = Attribute("status")

  override protected def parser: Parser[Availability] = for {
    status <- statusAttribute.optional
    xml <- Element.allNodes
  } yield new Availability(
    status,
    xml
  )

  override protected val antiparser: Antiparser[Availability] = Antiparser(
    // TODO why can't I remove [T] from premap() calls here?
    statusAttribute.toAntiparserOption.premap[Availability](_.status),
    Antiparser.xml.premap[Availability](_.xml)
  )
}
