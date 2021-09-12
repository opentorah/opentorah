package org.opentorah.xml

import org.xml.sax.{InputSource, XMLFilter}
import java.io.StringReader
import java.net.URL

// This abstracts over the XML model, allowing pretty-printing of both Scala XML and DOM.
trait Xml extends XmlAttributes:
  type Node
  final type Nodes = Seq[Node]
  override type Attributes <: Node
  final override type Element = Attributes
  type Text <: Node
  type Comment <: Node

  final type Predicate = Element => Boolean

  final def loadFromString(string: String, filters: Seq[XMLFilter] = Seq.empty, resolver: Option[Resolver] = None): Element =
    loadFromSource(InputSource(StringReader(string)), filters, resolver)

  final def loadFromUrl(url: URL, filters: Seq[XMLFilter] = Seq.empty, resolver: Option[Resolver] = None): Element =
    loadFromSource(Sax.url2inputSource(url), filters, resolver)

  protected def loadFromSource(source: InputSource, filters: Seq[XMLFilter], resolver: Option[Resolver]): Element

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

  final def isEmpty(element: Element): Boolean = isEmpty(getChildren(element))
  final def isEmpty(nodes: Nodes): Boolean = nodes.forall(isWhitespace)

  final def optional[T](option: Option[T])(f: T => Nodes): Nodes =
    option.fold[Nodes](Seq.empty)(value => f(value))

object Xml:
  val header: String   = """<?xml version="1.0" encoding="UTF-8"?>"""
  val header16: String = """<?xml version="1.0" encoding="UTF-16"?>"""

  val namespace: Namespace = Namespace(uri = "http://www.w3.org/XML/1998/namespace", prefix = "xml")

  val idAttribute: Attribute[String] = Attribute("id", namespace)
  val langAttribute: Attribute[String] = Attribute("lang", namespace)
