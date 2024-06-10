package org.opentorah.xml

import org.opentorah.util.Strings
import zio.ZIO

object Xml:
  val header: String = """<?xml version="1.0" encoding="UTF-8"?>"""
  val header16: String = """<?xml version="1.0" encoding="UTF-16"?>"""

  val namespace: Namespace = Namespace(uri = "http://www.w3.org/XML/1998/namespace", prefix = "xml")

  val idAttribute: Attribute[String] = Attribute("id", namespace)
  val langAttribute: Attribute[String] = Attribute("lang", namespace)
  val baseAttribute: Attribute[String] = Attribute("base", namespace)

  type Node = scala.xml.Node
  given CanEqual[scala.collection.immutable.Nil.type, Nodes] = CanEqual.derived // Note: just for case matching of Nil against Nodes...

  type Nodes = Seq[Node]

  type Element = scala.xml.Elem

  // Note: some whitespace is packaged not in Text, but in a different subclass of Atom[String], so:
  type Text = scala.xml.Atom[?]

  private type Comment = scala.xml.Comment

  def isText(node: Node): Boolean = node.isInstanceOf[Text]
  def asText(node: Node): Text = node.asInstanceOf[Text]
  def getText(text: Text): String = text.text
  def mkText(text: String): Text = scala.xml.Text(text)

  def isComment(node: Node): Boolean = node.isInstanceOf[Comment]
  def mkComment(text: String): Comment = scala.xml.Comment(s" $text ")

  final def isWhitespace(node: Node): Boolean = isText(node) && getText(asText(node)).trim.isEmpty
  final def isCharacters(node: Node): Boolean = isText(node) && getText(asText(node)).trim.nonEmpty

  def toString(node: Node): String = Strings.squashWhitespace(node match // TODO why the squash?
    case elem: Element => (elem.child map (_.text)).mkString(" ") // TODO hope this is not used: no tags, no attributes...

    // TODO is the rest any different from _.text?
    case text: Text => text.data.toString
    case special: scala.xml.SpecialNode => Strings.sbToString(special.buildString)
    case node: Node => node.text
  )

  val nodes: Parsable[Xml.Nodes] = new Parsable[Xml.Nodes]:
    override protected def parser: Parser[Xml.Nodes] = ParserState.allNodes
    override def unparser: Unparser[Xml.Nodes] = Unparser[Xml.Nodes](content = identity)
    
  final def toString(nodes: Nodes): String = nodes.map(toString).mkString(" ")

  def isElement(node: Node): Boolean = node.isInstanceOf[Element]
  def asElement(node: Node): Element = node.asInstanceOf[Element]
  def getName(element: Element): String = element.label
  def getPrefix(element: Element): Option[String] = Option(element.prefix)
  def rename(element: Element, name: String): Element = element.copy(label = name)

  def getChildren(element: Element): Nodes = element.child
  def setChildren(element: Element, children: Nodes): Element = element.copy(child = children)
  
  final def prependChildren(element: Element, nodes: Nodes): Element = setChildren(element, nodes ++ getChildren(element))
  final def appendChildren(element: Element, nodes: Nodes): Element = setChildren(element, getChildren(element) ++ nodes)
  
  final def isEmpty(element: Element): Boolean = isEmpty(getChildren(element))
  final def isEmpty(nodes: Nodes): Boolean = nodes.forall(isWhitespace)
  
  final def optional[T](option: Option[T])(f: T => Nodes): Nodes =
    option.fold[Nodes](Seq.empty)(f)

  final def conditional(condition: Boolean)(f: => Nodes): Nodes =
    if !condition then Seq.empty else f

  def descendants[T](nodes: Nodes, elementName: String, elements: Elements[T]): Parser[Seq[T]] = ZIO.foreach(
    descendants(nodes, elementName).filter(isElement).map[Element](asElement)
  )(descendant => elements.parse(From.xml("descendants", descendant)))

  protected def descendants(nodes: Nodes, elementName: String): Nodes = nodes.flatMap(node => node.flatMap(_ \\ elementName))

  def multi(nodes: Nodes, separator: String = ", "): Nodes = nodes match
    case Nil => Nil
    case n :: Nil => Seq(n)
    case n :: n1 :: ns if isElement(n) && isElement(n1) => Seq(n, mkText(separator)) ++ multi(n1 :: ns, separator)
    case n :: ns => Seq(n) ++ multi(ns, separator)
    case n => n
