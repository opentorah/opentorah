package org.opentorah.xml

import org.opentorah.util.Strings
import zio.{Runtime, URIO, ZIO}
import scala.xml.{MetaData, NamespaceBinding, Null, SpecialNode, TopScope}

object Xml extends Model {

  override type Node = scala.xml.Node
  override type Element = scala.xml.Elem
  // Note: some whitespace is packaged not in Text, but in a different subclass of Atom[String], so:
  override type Text = scala.xml.Atom[_]

  val namespace: Namespace = Namespace(uri = "http://www.w3.org/XML/1998/namespace", prefix = "xml")

  val header: String   = """<?xml version="1.0" encoding="UTF-8"?>"""
  val header16: String = """<?xml version="1.0" encoding="UTF-16"?>"""

  val idAttribute: Attribute[String] = Attribute("id", namespace)
  val langAttribute: Attribute[String] = Attribute("lang", namespace)

  type Transform[S] = Element => URIO[S, Element]

  def inNamespace[S](namespace: Namespace, transform: Transform[S]): Transform[S] = (element: Element) =>
    if (Namespace.get(element) != namespace.default) URIO.succeed(element) else transform(element)

  private def transformNode[S](transform: Transform[S])(node: Node): URIO[S, Node] =
    if (!isElement(node)) ZIO.succeed(node) else transform(asElement(node))

  // TODO can recursion here be simplified?
  private def depthFirst[S](transform: Transform[S]): Transform[S] = (element: Element) => for {
    newElement <- transform(element)
    children <- ZIO.foreach(getChildren(newElement))(transformNode(depthFirst(transform)))
  } yield newElement.copy(child = children)

  def runTransform[S](transform: Transform[S], state: S, element: Element): (Element, S) = {
    val result: URIO[S, (Element, S)] = for {
      resultElement <- depthFirst(transform)(element)
      resultState <- ZIO.access[S](identity)
    } yield (resultElement, resultState)

    Runtime.default.unsafeRun(result.provide(state))
  }

  def descendants(xml: Node, name: String): Seq[Element] =
    xml.flatMap(_ \\ name).filter(isElement).map(asElement)

  def multi(nodes: Nodes, separator: String = ", "): Nodes = nodes match {
    case Nil => Nil
    case n :: Nil => Seq(n)
    case n :: n1 :: ns if n.isInstanceOf[Element] && n1.isInstanceOf[Element] => Seq(n, mkText(separator)) ++ multi(n1 :: ns)
    case n :: ns => Seq(n) ++ multi(ns)
    case n => n
  }

  override def toString(node: Node): String = Strings.squashWhitespace {
    node match {
      case elem: Element => (elem.child map (_.text)).mkString(" ")
      case text: Text => text.data.toString
      case special: SpecialNode => Strings.sbToString(special.buildString)
      case node: Node => node.text
    }
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

  // TODO implement
  override def isNamespaceDeclared(namespace: Namespace, element: Element): Boolean = ???

  override def declareNamespace(namespace: Namespace, element: Element): Element =
    element.copy(scope = NamespaceBinding(namespace.getPrefix.orNull, namespace.getUri.get, element.scope))

  override protected def getAttributeValueString(attribute: Attribute[_], element: Element): Option[String] = {
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
        // TODO Note: in Scala 2.13, Seq is in scala.collection.immutable, but Scala XML still returns scala.collection.Seq -
        // hence the `value =>...`
      ).optional.withValue(Option(attribute.value).map(value => getAttributeValueText(value)))
    }

  // TODO WTF?
  private def getAttributeValueText(value: Nodes): String =
    Strings.sbToString(scala.xml.Utility.sequenceToXML(value, TopScope, _, stripComments = true))

  // TODO addAll() doesn't modify existing attributes; this should...
  override protected def setAttribute[T](attribute: Attribute[T], value: T, element: Element): Element =
    addAttributes(Seq(attribute.required.withValue(value)), element)

  override def setAttributes(attributes: Seq[Attribute.Value[_]], element: Element): Element =
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

  override def getChildren(element: Element): Nodes = element.child

  def construct(
    name: String,
    namespace: Option[Namespace],
    attributes: Seq[Attribute.Value[_]],
    children: Seq[Node]
  ): Element = {
    val base: Element = namespace.fold(<elem/>)(_.default.declare(<elem/>))
    setAttributes(attributes, base).copy(
      label = name,
      child = children
    )
  }
}
