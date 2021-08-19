package org.opentorah.xml

import org.opentorah.util.Collections

final class Unparser[A] private(
  val attributes: A => Attribute.Values,
  val content: A => Seq[ScalaXml.Node],
  val namespace: Option[Namespace]
)

object Unparser {

  def apply[A](
    attributes: A => Attribute.Values = (_: A) => Seq.empty,
    content   : A => ScalaXml.Nodes   = (_: A) => Seq.empty,
    namespace : Option[Namespace]     = None
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
