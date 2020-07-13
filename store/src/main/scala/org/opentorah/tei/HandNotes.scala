package org.opentorah.tei

import org.opentorah.xml.{Attribute, Element, Parser}
import scala.xml.Elem

final case class HandNotes(
  handNotes: Seq[HandNote.Value]
)

object HandNotes extends Element.WithToXml[HandNotes]("handNotes") {
  def apply(): HandNotes = new HandNotes(
    handNotes = Seq.empty
  )

  override protected def parser: Parser[HandNotes] = for {
    handNotes <- HandNote.parsable.all
  } yield new HandNotes(
    handNotes
  )

  override protected def attributes(value: HandNotes): Seq[Attribute.Value[_]] = Seq.empty

  override protected def content(value: HandNotes): Seq[Elem] =
    HandNote.parsable.toXml(value.handNotes)
}
