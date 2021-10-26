package org.opentorah.xml

import org.slf4j.{Logger, LoggerFactory}
import org.xml.sax.{InputSource, XMLFilter}
import java.io.StringReader
import java.net.URL
import zio.{URIO, ZIO}

// This abstracts over the XML model, allowing parsing and pretty-printing of both Scala XML and DOM.
trait Xml extends XmlAttributes:
  type Node
  given CanEqual[scala.collection.immutable.Nil.type, Nodes] = CanEqual.derived // Note: just for case matching of Nil agains Nodes...

  final type Nodes = Seq[Node]
  override type Attributes <: Node
  final override type Element = Attributes
  type Text <: Node
  type Comment <: Node
  
  final def loadFromString(string: String, filters: Seq[XMLFilter] = Seq.empty, resolver: Option[Resolver] = None): Element =
    loadFromInputSource(InputSource(StringReader(string)), filters, resolver)

  final def loadFromUrl(url: URL, filters: Seq[XMLFilter] = Seq.empty, resolver: Option[Resolver] = None): Element =
    loadFromInputSource(Sax.url2inputSource(url), filters, resolver)

  protected def loadFromInputSource(source: InputSource, filters: Seq[XMLFilter], resolver: Option[Resolver]): Element

  def isText(node: Node): Boolean
  def asText(node: Node): Text
  def getText(text: Text): String

  // Note: seed is the node used (for DOM) to get at the document so that a new node can be created.
  def mkText(text: String, seed: Node): Text
  def mkComment(text: String, seed: Node): Comment

  final def isWhitespace(node: Node): Boolean = isText(node) && getText(asText(node)).trim.isEmpty
  final def isCharacters(node: Node): Boolean = isText(node) && getText(asText(node)).trim.nonEmpty

  def toString(node: Node): String
  final def toString(nodes: Nodes): String = nodes.map(toString).mkString(" ")

  def isElement(node: Node): Boolean
  def asElement(node: Node): Element
  def getName(element: Element): String
  def getPrefix(element: Element): Option[String]

  def getChildren(element: Element): Nodes
  def setChildren(element: Element, children: Nodes): Element

  final def isEmpty(element: Element): Boolean = isEmpty(getChildren(element))
  final def isEmpty(nodes: Nodes): Boolean = nodes.forall(isWhitespace)

  final def optional[T](option: Option[T])(f: T => Nodes): Nodes =
    option.fold[Nodes](Seq.empty)(f)

  // TODO use more:)
  final def conditional(condition: Boolean)(f: => Nodes): Nodes =
    if (!condition) then Seq.empty else f

  final class Transform[R](transform: Element => URIO[R, Element]):
    val one: Element => URIO[R, Element] = element => for
      newElement: Element <- transform(element)
      children: Nodes <- URIO.foreach(getChildren(newElement))(
        (node: Node) => if !isElement(node) then URIO.succeed(node) else one(asElement(node))
      )
    yield setChildren(newElement, children)

    val all: Seq[Element] => URIO[R, Seq[Element]] = URIO.foreach(_)(one)

  def descendants[T](nodes: Nodes, elementName: String, elements: Elements[T]): Parser[Seq[T]] = ZIO.foreach(
    descendats(nodes, elementName).filter(isElement).map[Element](asElement)
  )(descendant => elements.parse(From.xml(this)("descendants", descendant)))

  protected def descendats(nodes: Nodes, elementName: String): Nodes

  def multi(nodes: Nodes, separator: String = ", "): Nodes = nodes match
    case Nil => Nil
    case n :: Nil => Seq(n)
    case n :: n1 :: ns if isElement(n) && isElement(n1) => Seq(n, mkText(separator, n)) ++ multi(n1 :: ns, separator)
    case n :: ns => Seq(n) ++ multi(ns, separator) 
    case n => n
      
object Xml:
  val logger: Logger = LoggerFactory.getLogger("org.opentorah.xml")

  val header: String   = """<?xml version="1.0" encoding="UTF-8"?>"""
  val header16: String = """<?xml version="1.0" encoding="UTF-16"?>"""

  val namespace: Namespace = Namespace(uri = "http://www.w3.org/XML/1998/namespace", prefix = "xml")

  val idAttribute: Attribute[String] = Attribute("id", namespace)
  val langAttribute: Attribute[String] = Attribute("lang", namespace)
