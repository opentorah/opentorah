package org.podval.judaica.parsers

import org.podval.judaica.viewer.{Content, DivContent, ViewerException}
import Xml.Ops
import scala.xml.{Elem, MetaData, Node, Text, TopScope, UnprefixedAttribute}

object DivContentParser {

  def prependAttribute(name: String, value: String, attributes: MetaData): MetaData =
    prependAttribute(name, Some(value), attributes)

  def prependAttribute(name: String, value: Option[String], attributes: MetaData): MetaData =
    value.fold(attributes)(v => new UnprefixedAttribute(name, Seq(Text(v)), attributes))

  def prependAttribute(name: String, value: Boolean, attributes: MetaData): MetaData =
    if (!value) attributes else new UnprefixedAttribute(name, Seq(Text("true")), attributes)

  def fromXml(xml: Elem): DivContent = {
    val sortOption = xml.attributeOption("type")
    if (sortOption.isEmpty) throw new ViewerException(s"No type for a div")

    val sort = sortOption.get
    val n: Option[String] = xml.attributeOption("n")
    val attributes = xml.attributes.filter(key => (key != "type") && (key != "n"))

    val hasHead = !xml.child.isEmpty && xml.child.head.isInstanceOf[Elem] && xml.child.head.label == "head"

    val head: Option[String] = if (!hasHead) None else Some(xml.child.head.text)

    val children: Seq[Content] = ContentParser.fromXmlSeq(if (!hasHead) xml.child else xml.child.tail)

    DivContent(sort, n, attributes, head, children)
  }

  def toXml(div: DivContent): Seq[Node] = {
    val prefixNodes = ContentParser.toXmlSeq(div.prefix)

    val headElemOption = div.head.map(head => <head>{head}</head>)

    val childrenNodes = headElemOption.toSeq ++ ContentParser.toXmlSeq(div.children)

    prefixNodes :+
      Elem(
        null,
        "div",
        prependAttribute("type", div.sort, prependAttribute("n", div.n, div.attributes)),
        TopScope,
        true,
        childrenNodes: _*)
  }
}
