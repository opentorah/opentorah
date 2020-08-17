package org.opentorah.xml

import org.opentorah.util.Collections
import scala.xml.Node

final class Antiparser[A] private(
  val attributes: A => Seq[Attribute.Value[_]],
  val content: A => Seq[Node]
) {

  def compose[B](f: B => A): Antiparser[B] = new Antiparser[B](
    attributes compose f,
    content compose f
  )
}

object Antiparser {

  def apply[A](
    attributes: A => Seq[Attribute.Value[_]] = (_: A) => Seq.empty,
    content   : A => Seq[Node]               = (_: A) => Seq.empty
  ): Antiparser[A] = new Antiparser[A](
    attributes,
    content
  )

  def apply[A](antiparsers: Antiparser[A]*): Antiparser[A] = apply[A](
    attributes = Collections.concat(antiparsers.map(_.attributes)),
    content    = Collections.concat(antiparsers.map(_.content   ))
  )

  val xml: Antiparser[Seq[Node]] = apply[Seq[Node]](
    content = (value: Seq[Node]) => value
  )
}
