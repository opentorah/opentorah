package org.opentorah.xml

import org.w3c.dom.Node

object Dom extends Model[Node] {
  override type Element = org.w3c.dom.Element
  override type Text = org.w3c.dom.CharacterData

  override def isText(node: Node): Boolean = node.isInstanceOf[Text]
  override def asText(node: Node): Text =    node.asInstanceOf[Text]
  override def getText(text: Text): String = text.getData
  override def mkText(text: String, seed: Node): Text = seed.getOwnerDocument.createTextNode(text)
  override def toString(node: Node): String = node.getTextContent

  override def isElement(node: Node): Boolean = node.isInstanceOf[Element]
  override def asElement(node: Node): Element = node.asInstanceOf[Element]
  override def getName(element: Element): String = element.getLocalName
  override def getPrefix(element: Element): Option[String] = Option(element.getPrefix)

  override def getAttributes(element: Element, parent: Option[Element]): Seq[Attribute.Value[String]] = {
    val list: org.w3c.dom.NamedNodeMap = element.getAttributes
    for (index <- 0 until list.getLength) yield {
      val attribute: org.w3c.dom.Attr = list.item(index).asInstanceOf[org.w3c.dom.Attr]
      Attribute(
        name = attribute.getLocalName,
        prefix = Option(attribute.getPrefix)
      ).withValue(Option(attribute.getValue))
    }
  }

  override def getChildren(element: Element): Seq[Node] = {
    val list: org.w3c.dom.NodeList = element.getChildNodes
    for (index <- 0 until list.getLength) yield list.item(index)
  }
}
