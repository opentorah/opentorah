package org.opentorah.tei

import org.opentorah.xml.{Attribute, ContentType, Element, Parser}
import scala.xml.Elem

final case class Pb(
  n: String,
  id: Option[String],
  facs: Option[String],
  isMissing: Boolean = false,
  isEmpty: Boolean = false
)

object Pb extends Element.WithToXml[Pb]("pb") {

  private val nAttribute: Attribute[String] = Attribute("n")
  private val missingAttribute: Attribute.BooleanAttribute = Attribute.BooleanAttribute("missing")
  private val emptyAttribute: Attribute.BooleanAttribute = Attribute.BooleanAttribute("empty")
  private val facsAttribute: Attribute[String] = Attribute("facs")

  override protected def contentType: ContentType = ContentType.Empty

  override protected def parser: Parser[Pb] = for {
    n <- nAttribute.required
    id <- Attribute.id.optional
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

  override protected def attributes(value: Pb): Seq[Attribute.Value[_]] = Seq(
    nAttribute.withValue(value.n),
    Attribute.id.withValue(value.id),
    facsAttribute.withValue(value.facs),
    missingAttribute.withNonDefaultValue(value.isMissing),
    emptyAttribute.withNonDefaultValue(value.isEmpty)
  )

  override protected def content(value: Pb): Seq[Elem] = Seq.empty
}
