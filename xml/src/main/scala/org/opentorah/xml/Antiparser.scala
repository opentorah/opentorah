package org.opentorah.xml

import org.opentorah.util.Collections
import scala.xml.Node

final class Antiparser[A] private(
  val attributes: A => Seq[Attribute.Value[_]],
  val content: A => Seq[Node],
  val namespace: Option[Namespace]
) {

  def compose[B](f: B => A): Antiparser[B] = new Antiparser[B](
    attributes compose f,
    content compose f,
    namespace
  )
}

object Antiparser {

  def apply[A](
    attributes: A => Seq[Attribute.Value[_]] = (_: A) => Seq.empty,
    content   : A => Seq[Node]               = (_: A) => Seq.empty,
    namespace : Option[Namespace]            = None
  ): Antiparser[A] = new Antiparser[A](
    attributes,
    content,
    namespace
  )

  def concat[A](
    antiparsers: Antiparser[A]*
  ): Antiparser[A] = concat[A](None, antiparsers)

  def concat[A](
    namespace: Option[Namespace],
    antiparsers: Seq[Antiparser[A]]
  ): Antiparser[A] = apply[A](
    attributes = Collections.concat(antiparsers.map(_.attributes)),
    content    = Collections.concat(antiparsers.map(_.content   )),
    namespace  = namespace
  )

  val xml: Antiparser[Seq[Node]] = apply[Seq[Node]](
    content = (value: Seq[Node]) => value
  )
}
