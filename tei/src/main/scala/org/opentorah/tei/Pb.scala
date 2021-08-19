package org.opentorah.tei

import org.opentorah.xml.{Unparser, Attribute, ContentType, Element, Parsable, Parser, ScalaXml, Xml}

final case class Pb(
  n: String,
  id: Option[String],
  facs: Option[String],
  isMissing: Boolean = false,
  isEmpty: Boolean = false
) {
  def addAttributes(element: ScalaXml.Element): ScalaXml.Element = ScalaXml.setAttributes(ScalaXml.getAttributes(element) ++ Seq(
    Pb.missingAttribute.withValue(isMissing),
    Pb.emptyAttribute.withValue(isEmpty)
  ), element)
}

object Pb extends Element[Pb]("pb") {

  def pageId(n: String): String = s"p$n"

  val nAttribute: Attribute.Required[String] = Attribute("n").required
  private val idAttribute: Attribute.Optional[String] = Xml.idAttribute.optional
  private val missingAttribute: Attribute.OrDefault[Boolean] = new Attribute.BooleanAttribute("missing").orDefault
  private val emptyAttribute: Attribute.OrDefault[Boolean] = new Attribute.BooleanAttribute("empty").orDefault
  private val facsAttribute: Attribute.Optional[String] = Attribute("facs").optional

  override def contentType: ContentType = ContentType.Empty

  override def contentParsable: Parsable[Pb] = new Parsable[Pb] {
    override val parser: Parser[Pb] = for {
      n <- nAttribute()
      id <- idAttribute()
      facs <- facsAttribute()
      isMissing <- missingAttribute()
      isEmpty <- emptyAttribute()
    } yield new Pb(
      n,
      id,
      facs,
      isMissing,
      isEmpty
    )

    override val unparser: Unparser[Pb] = Tei.concat(
      nAttribute(_.n),
      idAttribute(_.id),
      facsAttribute(_.facs),
      missingAttribute(_.isMissing),
      emptyAttribute(_.isEmpty)
    )
  }
}
