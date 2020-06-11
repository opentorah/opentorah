package org.opentorah.xml

import org.opentorah.util.Collections

final class UnionParsable[A](parsables: Seq[Parsable[A]]) extends Parsable[A] {

  Collections.checkNoDuplicates(parsables.flatMap(_.name2parser.keys),
    "element names among the parsers of the union")

  override def toString: Error =
    parsables.map(_.toString).mkString("[", "] or [", "]")

  override val name2parser: Map[String, Parsable.ContentTypeAndParser[A]] =
    parsables.flatMap(_.name2parser).toMap
}
