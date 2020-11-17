package org.opentorah.xml

// This abstracts over the XML model, allowing pretty-printing of both Scala XML and DOM.
trait Model extends PreModel {
  type Node
  final type Nodes = Seq[Node]
  override type Element <: Node
  final override type PreElement = Element
  type Text <: Node

  def isText(node: Node): Boolean
  def asText(node: Node): Text
  def getText(text: Text): String

  // Note: seed is the node used (for DOM) to get at the document so that a new node can be created.
  def mkText(text: String, seed: Node): Text

  final def isWhitespace(node: Node): Boolean = isText(node) && getText(asText(node)).trim.isEmpty
  final def isCharacters(node: Node): Boolean = isText(node) && getText(asText(node)).trim.nonEmpty

  def toString(node: Node): String
  final def toString(nodes: Nodes): String = nodes.map(toString).mkString("")

  def isElement(node: Node): Boolean
  def asElement(node: Node): Element
  def getName(element: Element): String
  def getPrefix(element: Element): Option[String]

  def getChildren(element: Element): Nodes

  final def isEmpty(element: Element): Boolean = isEmpty(getChildren(element))
  final def isEmpty(nodes: Nodes): Boolean = nodes.forall(isWhitespace)
}
