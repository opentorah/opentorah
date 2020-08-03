package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parser}

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

  override protected val parser: Parser[Pb] = for {
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

  override protected val antiparser: Antiparser[Pb] = Antiparser(
    nAttribute.toAntiparser.compose(_.n),
    Attribute.id.toAntiparserOption.compose(_.id),
    facsAttribute.toAntiparserOption.compose(_.facs),
    missingAttribute.toAntiparserNonDefault.compose(_.isMissing),
    emptyAttribute.toAntiparserNonDefault.compose(_.isEmpty)
  )
}
