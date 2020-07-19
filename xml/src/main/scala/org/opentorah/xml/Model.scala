package org.opentorah.xml

trait Model[N] {
  type NamespaceBinding // TODO merge with Namespace
  type Element <: N
  type Text <: N

  def isElement(node: N): Boolean
  def asElement(node: N): Element
  def getLabel(element: Element): String
  def isWhitespace(node: N): Boolean
  def isText(node: N): Boolean
  def getText(node: N): String
  def textNode(text: String): Text
  def isAtom(node: N): Boolean
  def getNodeText(node: N): String
  def topNamespaceBinding: NamespaceBinding
  def getNamespaceBinding(element: Element): NamespaceBinding
  def getNamespaceBindingString(element: Element, namespaceBinding: NamespaceBinding): String
  def getAttributes(element: Element): Seq[Attribute.Value[String]]
  def getChildren(element: Element): Seq[N]
  def getNameString(element: Element): String
}
