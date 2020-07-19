package org.opentorah.xml

import org.w3c.dom.Node

object Dom extends Model[Node] {
  override type Element = org.w3c.dom.Element
  override type Text = org.w3c.dom.CharacterData

  override def isAtom(node: Node): Boolean = ???
  override def isWhitespace(node: Node): Boolean = ???
  override def isCharacters(node: Node): Boolean = ???
  override def isText(node: Node): Boolean = node.isInstanceOf[Text]
  override def asText(node: Node): Text =    node.asInstanceOf[Text]
  override def getText(text: Text): String = ???
  override def mkText(text: String): Text = ???
  override def getNodeText(node: Node): String = ???
  override def isElement(node: Node): Boolean = node.isInstanceOf[Element]
  override def asElement(node: Node): Element = node.asInstanceOf[Element]
  override def getNamespaces(element: Element): Seq[Namespace] = ???
  override def getName(element: Element): String = ???
  override def getPrefix(element: Element): Option[String] = ???
  override def getAttributes(element: Element): Seq[Attribute.Value[String]] = ???
  override def getChildren(element: Element): Seq[Node] = ???
}
