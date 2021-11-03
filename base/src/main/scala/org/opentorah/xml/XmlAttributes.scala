package org.opentorah.xml

trait XmlAttributes:

  // Type of the attributes container suitable for getting.
  type Attributes

  // Type of the attributes container suitable for setting.
  type Element <: Attributes

  def getNamespace(attributes: Attributes): Namespace
  def getNamespaces(attributes: Attributes): Seq[Namespace]
  def isNamespaceDeclared(namespace: Namespace, attributes:  Attributes): Boolean
  def declareNamespace(namespace: Namespace, element: Element): Element

  final def ensureNamespaceDeclared(namespace: Namespace, element: Element): Unit =
    if !isNamespaceDeclared(namespace, element) then declareNamespace(namespace, element)

  final def getAttribute[T](attribute: Attribute[T], attributes: Attributes): Option[T] =
    attribute.get(getAttributeValueString(attribute, attributes))

  final def getAttributeWithDefault[T](attribute: Attribute[T], attributes: Attributes): T =
    getAttribute[T](attribute, attributes).getOrElse(attribute.default)

  final def doGetAttribute[T](attribute: Attribute[T], attributes: Attributes): T =
    getAttribute[T](attribute, attributes).get

  protected def getAttributeValueString(attribute: Attribute[?], attributes: Attributes): Option[String]

  def getAttributes(attributes: Attributes): Attribute.StringValues

  final def setAttribute[T](attributeValue: Attribute.Value[T], element: Element): Element =
    attributeValue.effectiveValue
      .map(value => setAttribute(attributeValue.attribute, value, element))
      .getOrElse(element)

  protected def setAttribute[T](attribute: Attribute[T], value: T, element: Element): Element

  def setAttributes(attributes: Attribute.Values, element: Element): Element

  // TODO rework addAttribute[s]:
  
  final def addAttribute(attribute: Attribute.Value[_], element: Element): Element =
    addAttributes(Seq(attribute), element)
    
  final def addAttributes(attributes: Attribute.Values, element: Element): Element =
    val existing: Attribute.Values = getAttributes(element)
    val toAdd: Attribute.Values = attributes
      .filterNot(toAdd => existing.exists(existing => existing.attribute.name == toAdd.attribute.name))

    setAttributes(existing ++ toAdd, element)

