package org.opentorah.tei

import org.opentorah.xml.{Attribute, ContentType, Element, ElementTo, Parsable, Parser, Unparser, Xml}

final class Pb(
  val n: String,
  val id: Option[String],
  val facs: Option[String],
  val isMissing: Boolean = false,
  val isEmpty: Boolean = false
):
  def addAttributes(element: Element): Element = Attribute.set(Attribute.get(element) ++ Seq(
    Pb.missingAttribute.withValue(isMissing),
    Pb.emptyAttribute.withValue(isEmpty)
  ), element)

object Pb extends ElementTo[Pb]("pb"):

  def pageId(n: String): String = s"p$n"

  val nAttribute: Attribute.Required[String] = Attribute("n").required
  private val idAttribute: Attribute.Optional[String] = Xml.idAttribute.optional
  private val missingAttribute: Attribute.OrDefault[Boolean] = Attribute.BooleanAttribute("missing").orDefault
  private val emptyAttribute: Attribute.OrDefault[Boolean] = Attribute.BooleanAttribute("empty").orDefault
  private val facsAttribute: Attribute.Optional[String] = Attribute("facs").optional

  override def contentType: ContentType = ContentType.Empty

  override def contentParsable: Parsable[Pb] = new Parsable[Pb]:
    override val parser: Parser[Pb] = for
      n: String <- nAttribute()
      id: Option[String] <- idAttribute()
      facs: Option[String] <- facsAttribute()
      isMissing: Boolean <- missingAttribute()
      isEmpty: Boolean <- emptyAttribute()
    yield Pb(
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
