package org.opentorah.xml

import org.opentorah.util.Strings
import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, MetaData, Node, Null, UnprefixedAttribute}

object Xml extends Model[Node] {
  override type NamespaceBinding = scala.xml.NamespaceBinding
  override type Element = scala.xml.Elem
  override type Text = scala.xml.Text

  val header: String   = """<?xml version="1.0" encoding="UTF-8"?>"""
  val header16: String = """<?xml version="1.0" encoding="UTF-16"?>"""

  type Transformer = Elem => Elem

  def transform(xml: Elem, transformer: Transformer): Elem = {
    val rule: RewriteRule = new RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case element: Elem => transformer(element)
        case other => other
      }
    }

    new RuleTransformer(rule).transform(xml).head.asInstanceOf[Elem]
  }

  def removeNamespace(xml: Elem): Elem =
    xml.copy(scope = scala.xml.TopScope, child = xml.child.map(removeNamespace))
  def removeNamespace(node: Node): Node = node match {
    case e: Elem => e.copy(scope = scala.xml.TopScope, child = e.child.map(removeNamespace))
    case n => n
  }

  def descendants(xml: Node, name: String): Seq[Elem] =
    xml.flatMap(_ \\ name).filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])

  def toString(nodes: Seq[Node]): String = nodes.map(toString).mkString("")
  def toString(node: Node): String = Strings.squashWhitespace {
    node match {
      case elem: Elem => (elem.child map (_.text)).mkString(" ")
      case node: Node => node.text
    }
  }

  override def isElement(node: Node): Boolean = node.isInstanceOf[Element]
  override def asElement(node: Node): Element = node.asInstanceOf[Element]
  override def getLabel(element: Element): String = element.label
  override def isWhitespace(node: Node): Boolean = isAtom(node) && node.text.trim.isEmpty
  override def isText(node: Node): Boolean = node.isInstanceOf[Text]
  override def getText(node: Node): String = node.asInstanceOf[Text].data
  override def textNode(text: String): scala.xml.Text = new scala.xml.Text(text)
  override def isAtom(node: Node): Boolean = node.isInstanceOf[scala.xml.Atom[_]]
  override def getNodeText(node: Node): String = node match {
    case special: scala.xml.SpecialNode => Strings.sbToString(special.buildString)
    case node: scala.xml.Node => node.text
  }

  override def topNamespaceBinding: NamespaceBinding = scala.xml.TopScope
  override def getNamespaceBinding(element: Element): NamespaceBinding = element.scope

  override def getNamespaceBindingString(element: Element, namespaceBinding: NamespaceBinding): String =
    element.scope.buildString(namespaceBinding).trim

  override def getAttributes(element: Element): Seq[Attribute.Value[String]] = element.attributes.toSeq
    .filter(_.isInstanceOf[scala.xml.Attribute])
    .map(_.asInstanceOf[scala.xml.Attribute])
    .map(attribute => Attribute(
      name = attribute.key,
      prefix = Option(attribute.pre)
    ).withValue(Option(attribute.value).map(getAttributeValueText)))

  // TODO maybe just value.text?
  private def getAttributeValueText(value: Seq[Node]): String =
    Strings.sbToString(scala.xml.Utility.appendQuoted(
      Strings.sbToString(scala.xml.Utility.sequenceToXML(value, scala.xml.TopScope, _, stripComments = true)), _))

  override def getChildren(element: Element): Seq[Node] = element.child

  override def getNameString(element: Element): String = Strings.sbToString(element.nameToString)

  def element(name: String, attributes: Seq[Attribute.Value[_]], content: Seq[Node]): Elem = <elem/>.copy(
    label = name,
    attributes = attributes.foldRight[MetaData](Null){ case (current, result) => new UnprefixedAttribute(
      current.attribute.name,
      current.valueToString.orNull,
      result
    )},
    child = content
  )
}
