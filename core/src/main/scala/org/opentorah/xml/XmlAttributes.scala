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

  // TODO in-place!
  final def ensureNamespaceDeclared(namespace: Namespace, element: Element): Unit =
    if !isNamespaceDeclared(namespace, element)
    then declareNamespace(namespace, element)

  def getAttribute(attribute: Attribute[?], attributes: Attributes): Option[String]

  def getAttributes(attributes: Attributes): Attribute.StringValues

  def setAttribute[T](attribute: Attribute.Value[T], element: Element): Element

  def setAttributes(attributes: Attribute.Values, element: Element): Element

  final def removeAttribute(attribute: Attribute[?], element: Element): Element =
    val attributes: Attribute.StringValues = getAttributes(element)
    setAttributes(attributes.filterNot(_.attribute.name == attribute.name), element)

  // TODO rework addAttribute[s]:
  
  final def addAttribute(attribute: Attribute.Value[?], element: Element): Element =
    addAttributes(Seq(attribute), element)
    
  final def addAttributes(attributes: Attribute.Values, element: Element): Element =
    val existing: Attribute.Values = getAttributes(element)
    val toAdd: Attribute.Values = attributes
      .filterNot(toAdd => existing.exists(existing => existing.attribute.name == toAdd.attribute.name))

    setAttributes(existing ++ toAdd, element)

