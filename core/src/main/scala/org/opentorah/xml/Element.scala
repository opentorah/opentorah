package org.opentorah.xml

type Element = scala.xml.Elem

object Element extends Node.Companion[Element]:
  def getName(element: Element): String = element.label
  def rename(element: Element, name: String): Element = element.copy(label = name)

  def getPrefix(element: Element): Option[String] = Option(element.prefix)

  def getChildren(element: Element): Nodes = element.child
  def setChildren(element: Element, children: Nodes): Element = element.copy(child = children)

  def prependChildren(element: Element, nodes: Nodes): Element = setChildren(element, nodes ++ getChildren(element))
  def appendChildren (element: Element, nodes: Nodes): Element = setChildren(element, getChildren(element) ++ nodes)

  def isEmpty(element: Element): Boolean = Nodes.isEmpty(getChildren(element))

  override def is(node: Node): Boolean = node.isInstanceOf[Element]
  override def as(node: Node): Element = node.asInstanceOf[Element]
