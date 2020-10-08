package org.opentorah.xml

import org.opentorah.util.Strings
import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{MetaData, NamespaceBinding, Node, Null, SpecialNode, TopScope}

object Xml extends Model[Node] {

  val namespace: Namespace = Namespace(uri = "http://www.w3.org/XML/1998/namespace", prefix = "xml")

  val header: String   = """<?xml version="1.0" encoding="UTF-8"?>"""
  val header16: String = """<?xml version="1.0" encoding="UTF-16"?>"""

  val idAttribute: Attribute[String] = Attribute("id", namespace)
  val langAttribute: Attribute[String] = Attribute("lang", namespace)

  def transform(xml: Element, transformer: Transformer): Element = {
    val rule: RewriteRule = new RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case element: Element => transformer(element)
        case other => other
      }
    }

    asElement(new RuleTransformer(rule).transform(xml).head)
  }

  def descendants(xml: Node, name: String): Seq[Element] =
    xml.flatMap(_ \\ name).filter(isElement).map(asElement)

  override type Element = scala.xml.Elem
  // Note: some whitespace is packaged not in Text, but in a different subclass of Atom[String], so:
  override type Text = scala.xml.Atom[_]

  override def toString(node: Node): String = Strings.squashWhitespace {
    node match {
      case elem: Element => (elem.child map (_.text)).mkString(" ")
      case text: Text => text.data.toString
      case special: SpecialNode => Strings.sbToString(special.buildString)
      case node: Node => node.text
    }
  }

  def multi(nodes: Seq[Node]): Seq[Node] = nodes match {
    case Nil => Nil
    case n :: Nil => Seq(n)
    case n :: ns if n.isInstanceOf[Element] => Seq(n, mkText(", ")) ++ multi(ns)
    case n :: ns => Seq(n) ++ multi(ns)
    case n => n
  }

  override def isText(node: Node): Boolean = node.isInstanceOf[Text]
  override def asText(node: Node): Text    = node.asInstanceOf[Text]
  override def getText(text: Text): String = text.text

  override def mkText(text: String, seed: Node): Text = mkText(text)
  def mkText(text: String): Text = new scala.xml.Text(text)

  override def isElement(node: Node): Boolean = node.isInstanceOf[Element]
  override def asElement(node: Node): Element = node.asInstanceOf[Element]

  override def getName(element: Element): String = element.label
  override def getPrefix(element: Element): Option[String] = Option(element.prefix)

  override def getNamespace(element: Element): Namespace = Namespace(
    prefix = element.prefix,
    uri = element.getNamespace(element.prefix)
  )

  override def getNamespaces(element: Element): Seq[Namespace] = {
    @scala.annotation.tailrec
    def get(result: Seq[Namespace], namespaceBinding: NamespaceBinding): Seq[Namespace] =
      if (namespaceBinding == TopScope) result else {
        val namespace: Namespace = Namespace(
          prefix = namespaceBinding.prefix,
          uri = namespaceBinding.uri
        )
        get(result :+ namespace, namespaceBinding.parent)
      }

    get(Seq.empty, element.scope)
  }

  override def declareNamespace(namespace: Namespace, element: Element): Element =
    element.copy(scope = NamespaceBinding(namespace.getPrefix.orNull, namespace.getUri.get, element.scope))

  // TODO remove
  def removeNamespace(element: Element): Element =
    element.copy(scope = TopScope, child = element.child.map(removeNamespace))

  // TODO remove
  def removeNamespace(node: Node): Node = node match {
    case element: Element => removeNamespace(element)
    case node => node
  }

  override def getAttribute(attribute: Attribute[_], element: Element): Option[String] = {
    val name: String = attribute.name
    val namespace: Namespace = attribute.namespace
    if (namespace.isDefault) element.attribute(name)
    else element.attribute(namespace.uri, name)
  }.map(_.text)

  override def getAttributes(element: Element): Seq[Attribute.Value[String]] = element.attributes.toSeq
    .filter(_.isInstanceOf[scala.xml.Attribute])
    .map(_.asInstanceOf[scala.xml.Attribute])
    .map { attribute =>
      val namespace: Namespace = Namespace(
        prefix = attribute.pre,
        uri = attribute.getNamespace(element)
      )
      Attribute(
        name = attribute.key,
        namespace = namespace
      ).withValue(Option(attribute.value).map(getAttributeValueText))
    }

  // TODO WTF?
  private def getAttributeValueText(value: Seq[Node]): String =
    Strings.sbToString(scala.xml.Utility.sequenceToXML(value, TopScope, _, stripComments = true))

  // TODO setAttribute() - and add it to Model

  def setAttributes(element: Element, attributes: Seq[Attribute.Value[_]]): Element =
    element.copy(attributes = attributes.foldRight[MetaData](Null)(
      (attributeValue, next) => toMetaData(attributeValue, next))
    )

  private def toMetaData[T](attributeValue: Attribute.Value[T], next: MetaData): MetaData = {
    val attribute: Attribute[T] = attributeValue.attribute
    val value: Option[T] = attributeValue.effectiveValue
    scala.xml.Attribute(
      pre = attribute.namespace.getPrefix.orNull,
      key = attribute.name,
      value = value.map(attribute.toString).map(Xml.mkText).map(Seq(_)).orNull,
      next = next
    )
  }

  override def getChildren(element: Element): Seq[Node] = element.child

  def construct(
    name: String,
    namespace: Option[Namespace],
    attributes: Seq[Attribute.Value[_]],
    children: Seq[Node]
  ): Element = {
    val base: Element = namespace.fold(<elem/>)(_.default.declare(<elem/>))
    setAttributes(base, attributes).copy(
      label = name,
      child = children
    )
  }
}
