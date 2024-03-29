package org.opentorah.xml

import org.slf4j.{Logger, LoggerFactory}
import org.xml.sax.{InputSource, XMLFilter}
import java.io.StringReader
import java.net.URL
import zio.{URIO, ZIO}

// This abstracts over the XML model, allowing parsing and pretty-printing of both Scala XML and DOM.
trait Xml extends XmlAttributes:
  type Node
  given CanEqual[scala.collection.immutable.Nil.type, Nodes] = CanEqual.derived // Note: just for case matching of Nil against Nodes...

  final type Nodes = Seq[Node]
  override type Attributes <: Node
  final override type Element = Attributes
  type Text <: Node
  type Comment <: Node

  // TODO just take XMLReader - but see transform()...
  def load(
    source: InputSource,
    filters: Seq[XMLFilter] = Seq.empty,
    resolver: Option[Resolver] = None,
    processIncludes: Xerces.ProcessIncludes = Xerces.ProcessIncludes.YesWithBases
  ): Element

  def isText(node: Node): Boolean
  def asText(node: Node): Text
  def getText(text: Text): String

  // Note: seed is the node used (for DOM) to get at the document so that a new node can be created.
  def mkText(text: String, seed: Node): Text

  def isComment(node: Node): Boolean
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

  final def prependChildren(element: Element, nodes: Nodes): Element = setChildren(element, nodes ++ getChildren(element))
  final def appendChildren(element: Element, nodes: Nodes): Element = setChildren(element, getChildren(element) ++ nodes)
  
  final def isEmpty(element: Element): Boolean = isEmpty(getChildren(element))
  final def isEmpty(nodes: Nodes): Boolean = nodes.forall(isWhitespace)

  final def allBases(element: Element): Seq[String] =
    Xml.baseAttribute.optional.get(this)(element).toSeq ++
    getChildren(element).filter(isElement).map(asElement).flatMap(allBases)

  final def optional[T](option: Option[T])(f: T => Nodes): Nodes =
    option.fold[Nodes](Seq.empty)(f)

  final def conditional(condition: Boolean)(f: => Nodes): Nodes =
    if !condition then Seq.empty else f

  def descendants[T](nodes: Nodes, elementName: String, elements: Elements[T]): Parser[Seq[T]] = ZIO.foreach(
    descendants(nodes, elementName).filter(isElement).map[Element](asElement)
  )(descendant => elements.parse(From.xml(this)("descendants", descendant)))

  protected def descendants(nodes: Nodes, elementName: String): Nodes

  def multi(nodes: Nodes, separator: String = ", "): Nodes = nodes match
    case Nil => Nil
    case n :: Nil => Seq(n)
    case n :: n1 :: ns if isElement(n) && isElement(n1) => Seq(n, mkText(separator, n)) ++ multi(n1 :: ns, separator)
    case n :: ns => Seq(n) ++ multi(ns, separator)
    case n => n

  def parentBase: Parser[Option[URL]]
  
object Xml:
  val logger: Logger = LoggerFactory.getLogger("org.opentorah.xml") // TODO eliminate

  val header: String   = """<?xml version="1.0" encoding="UTF-8"?>"""
  val header16: String = """<?xml version="1.0" encoding="UTF-16"?>"""

  val namespace: Namespace = Namespace(uri = "http://www.w3.org/XML/1998/namespace", prefix = "xml")

  val idAttribute: Attribute[String] = Attribute("id", namespace)
  val langAttribute: Attribute[String] = Attribute("lang", namespace)
  val baseAttribute: Attribute[String] = Attribute("base", namespace)
