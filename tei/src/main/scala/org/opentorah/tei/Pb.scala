package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parser, Xml}
import scala.xml.Elem

final case class Pb(
  n: String,
  id: Option[String],
  facs: Option[String],
  isMissing: Boolean = false,
  isEmpty: Boolean = false
) {
  def addAttributes(element: Elem): Elem = Xml.setAttributes(Xml.getAttributes(element) ++ Seq(
    Pb.missingAttribute.withOptionalValue(Some(isMissing)),
    Pb.emptyAttribute.withOptionalValue(Some(isEmpty))
  ), element)
}

object Pb extends Element.WithToXml[Pb]("pb") {

  val nAttribute: Attribute[String] = Attribute("n")
  private val missingAttribute: Attribute.BooleanAttribute = new Attribute.BooleanAttribute("missing")
  private val emptyAttribute: Attribute.BooleanAttribute = new Attribute.BooleanAttribute("empty")
  private val facsAttribute: Attribute[String] = Attribute("facs")

  override protected def contentType: ContentType = ContentType.Empty

  override protected val parser: Parser[Pb] = for {
    n <- nAttribute.required
    id <- Xml.idAttribute.optional
    facs <- facsAttribute.optional
    isMissing <- missingAttribute.optionalOrDefault
    isEmpty <- emptyAttribute.optionalOrDefault
  } yield new Pb(
    n,
    id,
    facs,
    isMissing,
    isEmpty
  )

  override protected val antiparser: Antiparser[Pb] = Tei.concat(
    nAttribute.toXml.compose(_.n),
    Xml.idAttribute.toXmlOption.compose(_.id),
    facsAttribute.toXmlOption.compose(_.facs),
    missingAttribute.toXml.compose(_.isMissing),
    emptyAttribute.toXml.compose(_.isEmpty)
  )
}
