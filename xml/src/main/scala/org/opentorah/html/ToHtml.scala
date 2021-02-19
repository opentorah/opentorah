package org.opentorah.html

import org.opentorah.html.endnotes.EndNotes
import org.opentorah.xml.{Attribute, Namespace, Xml}
import zio.{Has, URIO}

// Converting other XML dialects (e.g., TEI) to HTML
trait ToHtml[R <: Has[_]] {
  protected def namespace: Namespace

  protected def isEndNote(element: Xml.Element): Boolean

  protected def elementTransform(element: Xml.Element): URIO[R, Xml.Element]

  final def toHtml(element: Xml.Element): URIO[R, Xml.Element] = {
    def result: URIO[EndNotes with R, Xml.Element] = for {
      mainDiv <- transform.one(element)
      endNotesRaw <- endnotes.getEndNotes
      endNotesDone <- transform.all(endNotesRaw)
      /* TODO do not ignore end-notes inside end-notes */
    } yield
      // TODO do we need HTML namespace here?
      <div xmlns={Html.namespace.uri} class="html">
        {mainDiv}
        <div xmlns={Html.namespace.uri} class="endnotes">
          {endNotesDone}
        </div>
      </div>

    result.provideSomeLayer[R](endnotes.EndNotes.empty)
  }

  private def transform: Xml.Transform[EndNotes with R] =
    new Xml.Transform(element => elementTransformEffective(element).map(transformElement(element)))

  private def elementTransformEffective(element: Xml.Element): URIO[EndNotes with R, Xml.Element] =
    if (Namespace.get(element) != namespace.default) URIO.succeed(element) else
    if (isEndNote(element)) endnotes.addEndNote(
      id = Xml.idAttribute.optional.get(element),
      content = Xml.getChildren(element)
    ) else
      elementTransform(element)

  private def transformElement(element: Xml.Element)(newElement: Xml.Element): Xml.Element = {
    val name: String = element.label
    val attributes: Seq[Attribute.Value[String]] = Xml.getAttributes(element)
    if (newElement eq element) Xml.setAttributes(
      element = if (Html.reservedElements.contains(name)) element.copy(label = addPrefix(name)) else element,
      attributes = attributes.map(transformXmlAttribute)
    ) else Xml.addAttributes(
      element = Html.namespace.default.declare(newElement),
      attributes = Html.classAttribute.required.withValue(name) +: attributes.map(transformAttribute)
    )
  }

  private def transformAttribute(attribute: Attribute.Value[String]): Attribute.Value[String] = {
    val result: Attribute.Value[String] = transformXmlAttribute(attribute)
    val name: String = result.attribute.name
    if (!Html.reservedAttributes.contains(name)) result
    else Attribute(addPrefix(name)).optional.withValue(result.value)
  }

  private def transformXmlAttribute(attribute: Attribute.Value[String]): Attribute.Value[String] =
  // TODO xml:base? xml:space?
    if (attribute.attribute == Xml.idAttribute  ) Html.idAttribute  .optional.withValue(attribute.value) else
    if (attribute.attribute == Xml.langAttribute) Html.langAttribute.optional.withValue(attribute.value) else
      attribute

  private def addPrefix(name: String): String = s"${namespace.getPrefix.get}-$name"
}
