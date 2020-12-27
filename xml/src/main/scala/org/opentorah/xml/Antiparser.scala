package org.opentorah.xml

import org.opentorah.util.Collections

final class Antiparser[A] private(
  val attributes: A => Seq[Attribute.Value[_]],
  val content: A => Seq[Xml.Node],
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
    content   : A => Seq[Xml.Node]               = (_: A) => Seq.empty,
    namespace : Option[Namespace]            = None
  ): Antiparser[A] = new Antiparser[A](
    attributes,
    content,
    namespace
  )

  def concat[A](
    antiparsers: Antiparser[A]*
  ): Antiparser[A] = concat[A](None, antiparsers)

  def concatInNamespace[A](
    namespace: Namespace,
    antiparsers: Seq[Antiparser[A]]
  ): Antiparser[A] = concat[A](Some(namespace), antiparsers)

  private def concat[A](
    namespace: Option[Namespace],
    antiparsers: Seq[Antiparser[A]]
  ): Antiparser[A] = apply[A](
    attributes = Collections.concat(antiparsers.map(_.attributes)),
    content    = Collections.concat(antiparsers.map(_.content   )),
    namespace  = namespace
  )

  // TODO eliminate?
  private val xml: Antiparser[Seq[Xml.Node]] = apply[Seq[Xml.Node]](
    content = (value: Seq[Xml.Node]) => value
  )

  def xml[B](f: B => Seq[Xml.Node]): Antiparser[B] = xml.compose(f)
}
