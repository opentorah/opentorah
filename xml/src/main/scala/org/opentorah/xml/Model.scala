package org.opentorah.xml

trait Model[N] {
  type NamespaceBinding // TODO merge with Namespace
  type Element <: N
  type Text <: N

  def isAtom(node: N): Boolean
  def isWhitespace(node: N): Boolean
  def isText(node: N): Boolean
  def asText(node: N): Text
  def getText(text: Text): String
  def mkText(text: String): Text
  def getNodeText(node: N): String
  def topNamespaceBinding: NamespaceBinding
  def getNamespaceBinding(element: Element): NamespaceBinding
  def getNamespaceBindingString(elementNamespace: NamespaceBinding, namespace: NamespaceBinding): String
  def isElement(node: N): Boolean
  def asElement(node: N): Element
  def getLabel(element: Element): String
  def getAttributes(element: Element): Seq[Attribute.Value[String]]
  def getChildren(element: Element): Seq[N]
  def getNameString(element: Element): String // TODO dissolve
}
