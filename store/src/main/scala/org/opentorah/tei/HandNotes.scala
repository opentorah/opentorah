package org.opentorah.tei

import org.opentorah.xml.{Element, ToXml}
import scala.xml.Elem

final case class HandNotes(
  handNotes: Seq[HandNote.Value]
)

object HandNotes extends Element[HandNotes](
  elementName = "handNotes",
  parser = for {
    handNotes <- HandNote.parsable.all
  } yield new HandNotes(
    handNotes
  )
) with ToXml[HandNotes] {
  def apply(): HandNotes = new HandNotes(
    handNotes = Seq.empty
  )

  override def toXml(value: HandNotes): Elem =
    <handNotes>
      {HandNote.parsable.toXml(value.handNotes)}
    </handNotes>
}
