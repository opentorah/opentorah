package org.opentorah.tei

import org.opentorah.xml.{Attribute, ContentType, Element, ToXml}
import scala.xml.Elem

final case class Pb(
  n: String,
  id: Option[String],
  facs: Option[String],
  isMissing: Boolean = false,
  isEmpty: Boolean = false
)

object Pb extends Element[Pb](
  elementName = "pb",
  contentType = ContentType.Empty,
  parser = for {
    n <- Attribute("n").required
    id <- Attribute.id.optional
    facs <- Attribute("facs").optional
    isMissing <- Attribute("missing").boolean.orFalse
    isEmpty <- Attribute("empty").boolean.orFalse
  } yield new Pb(
    n,
    id,
    facs,
    isMissing,
    isEmpty
  )
) with ToXml[Pb] {

  override def toXml(value: Pb): Elem =
    <pb
      n={value.n}
      id={value.id.orNull}
      facs={value.facs.orNull}
      missing={if (value.isMissing) "true" else null}
      empty={if (value.isEmpty) "true" else null}
    />
}
