package org.opentorah.tei

import org.opentorah.xml.{Attribute, ContentType, Element}
import scala.xml.Elem

final case class Pb(
  n: String,
  id: Option[String],
  facs: Option[String]
)

object Pb extends Element[Pb](
  elementName = "pb",
  contentType = ContentType.Empty,
  parser = for {
    n <- Attribute("n").required
    id <- Attribute.id.optional
    facs <- Attribute("facs").optional
  } yield new Pb(
    n,
    id,
    facs
  )
) {
  override def toXml(value: Pb): Elem =
    <pb n={value.n} id={value.id.orNull} facs={value.facs.orNull}/>
}
