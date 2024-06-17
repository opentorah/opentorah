package org.opentorah.xml

abstract class ElementTo[A](val elementName: String) extends ElementsTo[A]:
  final override type ElementType = A

  final override protected def elements: Seq[ElementTo[A]] = Seq(this)

  final override protected def elementByValue(value: A): ElementTo[A] = this

  final override def map(elementTo: ElementTo[? <: A], parser: Parser[A]): Parser[A] = parser

  // TODO allow declaring this element's namespace - and remove namespace from Unparser?
  final def xmlElement(value: A): Element = contentParsable.unparser.xmlElement(elementName, value)

  override def toString: String = s"element '$elementName' [$contentType]"

  def contentType: ContentType = ContentType.Elements

  def contentParsable: Parsable[A]
