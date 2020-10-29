package org.opentorah.xml

import org.opentorah.xml.Xml.StateTransformer

object Xhtml extends Dialect {

  override val namespace: Namespace = Namespace(uri="http://www.w3.org/1999/xhtml", prefix="xhtml")

  override val mimeType: String = "application/xhtml+xml"

  val idAttribute: Attribute[String] = Attribute("id")
  val langAttribute: Attribute[String] = Attribute("lang")
  val classAttribute: Attribute[String] = Attribute("class")

  private val htmlElementNames: Set[String] = Set("head", "body")
  private val htmlAttributeNames: Set[String] = Set("class", "target", "href")

  def lift[S](elementTransformer: StateTransformer[S], fromNamespace: Namespace): StateTransformer[S] = (element, state) =>
    if (Namespace.get(element) != fromNamespace.default) (element, state) else {
      val (newElement, newState) = elementTransformer(element, state)
      val name: String = element.label
      val attributes = Attribute.getAll(element)
      val resultElement =
        if (newElement eq element) Attribute.setAll(
          element = if (htmlElementNames.contains(name)) element.copy(label = addPrefix(fromNamespace, name)) else element,
          attributes = attributes.map(transformXmlAttribute)
        ) else Attribute.addAll(
          element = namespace.default.declare(newElement),
          attributes = classAttribute.withValue(name) +: attributes.map(transformAttribute(fromNamespace, _))
        )
      (resultElement, newState)
    }

  private def transformAttribute(
    fromNamespace: Namespace,
    attribute: Attribute.Value[String]
  ): Attribute.Value[String] = {
    val result = transformXmlAttribute(attribute)
    val name: String = result.attribute.name
    if (htmlAttributeNames.contains(name))
      Attribute(addPrefix(fromNamespace, name)).withOptionalValue(result.value)
    else
      result
  }

  private def transformXmlAttribute(attribute: Attribute.Value[String]): Attribute.Value[String] =
    if (attribute.attribute == Xml.idAttribute  ) idAttribute  .withOptionalValue(attribute.value) else
    if (attribute.attribute == Xml.langAttribute) langAttribute.withOptionalValue(attribute.value) else
      attribute


  private def addPrefix(namespace: Namespace, name: String): String =
    s"${namespace.getPrefix.get}-$name"
}
