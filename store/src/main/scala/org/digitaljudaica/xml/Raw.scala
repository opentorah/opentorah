package org.digitaljudaica.xml

import zio.ZIO
import scala.xml.Elem

// TODO dissolve - unless used in calendar...
final class Raw(name: Option[String]) extends Parsable[Elem] {
  override def toString: String = s"raw $name"

  private val takeNextElement: Parser[Option[Elem]] =
    Context.liftContentModifier(Content.takeNextElement)

  override val optional: Parser[Option[Elem]] = name.fold(takeNextElement) { name =>
    for {
      has <- Xml.nextNameIs(name)
      result <- if (!has) ZIO.none else takeNextElement
    } yield result
  }

  override val all: Parser[Seq[Elem]] =
    name.fold(Context.liftContentModifier(Content.takeAllElements))(_ => super.all)
}

object Raw {
  def apply(name: String): Parsable[Elem] =
    new Raw(Some(name))

  def apply(): Parsable[Elem] =
    new Raw(None)
}
