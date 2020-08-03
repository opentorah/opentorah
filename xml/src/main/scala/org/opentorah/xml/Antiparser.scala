package org.opentorah.xml

import scala.xml.Node

final class Antiparser[A] private(
  val attributes: A => Seq[Attribute.Value[_]],
  val content: A => Seq[Node]
) {

  // TODO rename 'compose'
  def premap[B](f: B => A): Antiparser[B] = new Antiparser[B](
    attributes compose f,
    content compose f
  )
}

object Antiparser {

  // TODO remove
  def apply[A](
    attributes: A => Seq[Attribute.Value[_]] = (_: A) => Seq.empty,
    content: A => Seq[Node] = (_: A) => Seq.empty
  ): Antiparser[A] = new Antiparser[A](
    attributes,
    content
  )

  def apply[A](antiparsers: Antiparser[A]*): Antiparser[A] = new Antiparser[A](
    attributes = concat(antiparsers.map(_.attributes)),
    content = concat(antiparsers.map(_.content))
  )

  // TODO what is this in pointless notation?
  def concat[A, B](fs: Seq[A => Seq[B]]): A => Seq[B] = a => fs.flatMap(f => f(a))

  val xml: Antiparser[Seq[Node]] = apply[Seq[Node]](
    content = (value: Seq[Node]) => value
  )
}
