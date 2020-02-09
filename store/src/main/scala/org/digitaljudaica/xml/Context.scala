package org.digitaljudaica.xml

import scala.xml.Elem

final case class Context(stack: List[Element]) {

  def current: Element = stack.head

  def getName: String = current.name

  def getAttribute(name: String): Option[String] = current.getAttribute(name)

  def forgetAttribute(name: String): Context = modifyCurrent(_.forgetAttribute(name))

  def getElements: Seq[Elem] = current.elements

  def getCharacters: Option[String] = current.characters

  def forgetCharacters: Context = modifyCurrent(_.forgetCharacters)

  def getNextNestedElementName: Option[String] = current.getNextNestedElementName

  def getNumberOfNestedElementsWithName(name: String): Int = current.getNumberOfNestedElementsWithName(name)

  def push(url: Option[String], elem: Elem): Context = copy(stack = Element(url, elem) :: stack)

  def pushNextNestedElement: Context = {
    val toPush = current.getNextNestedElement
    modifyCurrent(_.moveToNextNestedElement).push(url = None, toPush)
  }

  private def modifyCurrent(f: Element => Element): Context =
    copy(stack = f(stack.head) :: stack.tail)

  def pop: Context = copy(stack = stack.tail)
}
