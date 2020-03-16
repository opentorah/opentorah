package org.opentorah.xml

final class UnionParsable[A](parsables: Seq[Parsable[A]]) extends Parsable[A] {

  // TODO require that name2parser maps do not overlap

  override def toString: Error =
    parsables.map(_.toString).mkString("[", "] or [", "]")

  def name2parser: Map[String, Parsable.ContentTypeAndParser[A]] =
    parsables.flatMap(_.name2parser).toMap
}
