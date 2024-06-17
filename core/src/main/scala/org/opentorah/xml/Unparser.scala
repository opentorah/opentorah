package org.opentorah.xml

import org.opentorah.util.Collections

final class Unparser[A](
  val attributes: A => Attribute.Values = (_: A) => Seq.empty,
  val content   : A => Nodes            = (_: A) => Seq.empty,
  val namespace : Option[Namespace]     = None // TODO this is mostly ignored
):
  def xmlElement(elementName: String, value: A): Element =
    val element: Element = Element.rename(<elem/>, elementName)
    Element.setChildren(Attribute.set(
      values = attributes(value),
      element = namespace.fold(element)(_.default.declare(element))
    ),
      children = content(value)
    )

object Unparser:

  def concat[A](
    unparsers: Unparser[A]*
  ): Unparser[A] = concat[A](None, unparsers)

  def concatInNamespace[A](
    namespace: Namespace,
    unparsers: Seq[Unparser[A]]
  ): Unparser[A] = concat[A](Some(namespace), unparsers)

  private def concat[A](
    namespace: Option[Namespace],
    unparsers: Seq[Unparser[A]]
  ): Unparser[A] = Unparser[A](
    attributes = Collections.concat(unparsers.map(_.attributes)),
    content    = Collections.concat(unparsers.map(_.content   )),
    namespace  = namespace
  )
