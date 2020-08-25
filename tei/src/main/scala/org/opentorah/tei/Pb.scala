package org.opentorah.tei

import org.opentorah.util.Files
import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parser, Xml}

final case class Pb(
  n: String,
  id: Option[String],
  facs: Option[String],
  isMissing: Boolean = false,
  isEmpty: Boolean = false
)

object Pb extends Element.WithToXml[Pb]("pb") {

  private val nAttribute: Attribute[String] = Attribute("n")
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

  def transformer(resolver: TeiResolver): Xml.Transformer =
    elem => if (elem.label != elementName) elem else {
      val pageId: String = Page.pageId(nAttribute.doGet(elem))
      <pb
        xml:id={pageId}
        rendition={Page.pageRendition(
          isMissing = missingAttribute.getWithDefault(elem),
          isEmpty = emptyAttribute.getWithDefault(elem)
        )}
        role={resolver.facs.role.orNull}
        target={Files.mkUrl(Files.addPart(resolver.facs.url, pageId))}
      />
    }
}
