package org.opentorah.xml

// This abstracts over the XML model, allowing pretty-printing of both Scala XML and DOM.
// TODO split out attributes-related stuff into a separate trait and make Model and Sax inherit from it.
trait Model[N] {
  type Element <: N
  type Text <: N

  def isText(node: N): Boolean
  def asText(node: N): Text
  def getText(text: Text): String

  // Note: seed is the node used (for DOM) to get at the document so that a new node can be created.
  def mkText(text: String, seed: N): Text

  final def isWhitespace(node: N): Boolean = isText(node) && getText(asText(node)).trim.isEmpty
  final def isCharacters(node: N): Boolean = isText(node) && getText(asText(node)).trim.nonEmpty

  def toString(node: N): String
  final def toString(nodes: Seq[N]): String = nodes.map(toString).mkString("")

  def isElement(node: N): Boolean
  def asElement(node: N): Element
  def getName(element: Element): String
  def getPrefix(element: Element): Option[String]

  def getNamespace(element: Element): Namespace
  def getNamespaces(element: Element): Seq[Namespace]
  // TODO isNamespaceDeclared()
  def declareNamespace(namespace: Namespace, element: Element): Element

  // TODO _ or String? Value?
  def getAttribute(attribute: Attribute[_], element: Element): Option[String]
  def getAttributes(element: Element): Seq[Attribute.Value[String]]

  final def setAttribute[T](attributeValue: Attribute.Value[T], element: Element): Element =
    attributeValue.effectiveValue
      .map(value => setAttribute(attributeValue.attribute, value, element))
      .getOrElse(element)

  protected def setAttribute[T](attribute: Attribute[T], value: T, element: Element): Element

  def setAttributes(attributes: Seq[Attribute.Value[_]], element: Element): Element

  final def addAttributes(attributes: Seq[Attribute.Value[_]], element: Element): Element = {
    val existing: Seq[Attribute.Value[_]] = getAttributes(element)
    val toAdd: Seq[Attribute.Value[_]] = attributes
      .filterNot(toAdd => existing.exists(existing => existing.attribute.name == toAdd.attribute.name))

    setAttributes(existing ++ toAdd, element)
  }

  def getChildren(element: Element): Seq[N]

  final def isEmpty(element: Element): Boolean = isEmpty(getChildren(element))
  final def isEmpty(nodes: Seq[N]): Boolean = nodes.forall(isWhitespace)
}
