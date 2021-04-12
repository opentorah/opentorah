package org.opentorah.xml

import org.opentorah.util.Collections

final class Unparser[A] private(
  val attributes: A => Seq[Attribute.Value[_]],
  val content: A => Seq[Xml.Node],
  val namespace: Option[Namespace]
)

object Unparser {

  def apply[A](
    attributes: A => Seq[Attribute.Value[_]] = (_: A) => Seq.empty,
    content   : A => Seq[Xml.Node]           = (_: A) => Seq.empty,
    namespace : Option[Namespace]            = None
  ): Unparser[A] = new Unparser[A](
    attributes,
    content,
    namespace
  )

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
  ): Unparser[A] = apply[A](
    attributes = Collections.concat(unparsers.map(_.attributes)),
    content    = Collections.concat(unparsers.map(_.content   )),
    namespace  = namespace
  )
}
