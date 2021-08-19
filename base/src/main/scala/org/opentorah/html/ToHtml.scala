package org.opentorah.html

import org.opentorah.xml.{Attribute, Namespace, ScalaXml, Xml}
import zio.{Has, URIO}

// Converting other XML dialects (e.g., TEI, DocBook) to HTML
trait ToHtml[R <: Has[_]] {
  protected def namespace: Namespace

  protected def isEndNote(element: ScalaXml.Element): Boolean

  protected def elementTransform(element: ScalaXml.Element): URIO[R, ScalaXml.Element]

  final def toHtml(element: ScalaXml.Element): URIO[R, ScalaXml.Element] = {
    def result: URIO[Has[EndNotes] with R, ScalaXml.Element] = for {
      mainDiv <- transform.one(element)
      endNotesRaw <- EndNotes.getEndNotes
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

    result.provideSomeLayer[R](EndNotes.empty)
  }

  private def transform: ScalaXml.Transform[Has[EndNotes] with R] =
    new ScalaXml.Transform(element => elementTransformEffective(element).map(transformElement(element)))

  private def elementTransformEffective(element: ScalaXml.Element): URIO[Has[EndNotes] with R, ScalaXml.Element] =
    if (ScalaXml.getNamespace(element) != namespace.default) URIO.succeed(element) else
    if (isEndNote(element)) EndNotes.addEndNote(
      id = Xml.idAttribute.optional.get(ScalaXml)(element),
      content = ScalaXml.getChildren(element)
    ) else
      elementTransform(element)

  private def transformElement(element: ScalaXml.Element)(newElement: ScalaXml.Element): ScalaXml.Element = {
    val name: String = ScalaXml.getName(element)
    val attributes: Seq[Attribute.Value[String]] = ScalaXml.getAttributes(element)
    if (newElement eq element) ScalaXml.setAttributes(
      element = if (Html.reservedElements.contains(name)) element.copy(label = addPrefix(name)) else element,
      attributes = attributes.map(transformXmlAttribute)
    ) else ScalaXml.addAttributes(
      element = ScalaXml.declareNamespace(Html.namespace.default, newElement),
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
