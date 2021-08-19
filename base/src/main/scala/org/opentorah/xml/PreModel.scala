package org.opentorah.xml

trait PreModel {

  // Type of the attributes container suitable for getting.
  type PreElement // TODO rename Attributes and use where org.sax.Attributes is used now...

  // Type of the attributes container suitable for setting.
  type Element <: PreElement

  def getNamespace(element: PreElement): Namespace
  def getNamespaces(element: PreElement): Seq[Namespace]
  def isNamespaceDeclared(namespace: Namespace, element:  PreElement): Boolean
  def declareNamespace(namespace: Namespace, element: Element): Element

  final def getAttribute[T](attribute: Attribute[T], element: PreElement): Option[T] =
    attribute.get(getAttributeValueString(attribute, element))

  final def getAttributeWithDefault[T](attribute: Attribute[T], element: PreElement): T =
    getAttribute[T](attribute, element).getOrElse(attribute.default)

  final def doGetAttribute[T](attribute: Attribute[T], element: PreElement): T =
    getAttribute[T](attribute, element).get

  protected def getAttributeValueString(attribute: Attribute[_], element: PreElement): Option[String]

  def getAttributes(element: PreElement): Seq[Attribute.Value[String]]

  final def setAttribute[T](attributeValue: Attribute.Value[T], element: Element): Element =
    attributeValue.effectiveValue
      .map(value => setAttribute(attributeValue.attribute, value, element))
      .getOrElse(element)

  protected def setAttribute[T](attribute: Attribute[T], value: T, element: Element): Element

  def setAttributes(attributes: Attribute.Values, element: Element): Element

  final def addAttributes(attributes: Attribute.Values, element: Element): Element = {
    val existing: Attribute.Values = getAttributes(element)
    val toAdd: Attribute.Values = attributes
      .filterNot(toAdd => existing.exists(existing => existing.attribute.name == toAdd.attribute.name))

    setAttributes(existing ++ toAdd, element)
  }
}
