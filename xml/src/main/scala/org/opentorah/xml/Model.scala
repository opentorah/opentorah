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

  def getAttribute(attribute: Attribute[_], element: Element): Option[String]
  def getAttributes(element: Element): Seq[Attribute.Value[String]]

  def getChildren(element: Element): Seq[N]

  final def isEmpty(element: Element): Boolean = isEmpty(getChildren(element))
  final def isEmpty(nodes: Seq[N]): Boolean = nodes.forall(isWhitespace)
}
