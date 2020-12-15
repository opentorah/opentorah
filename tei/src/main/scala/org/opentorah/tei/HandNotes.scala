package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parser}

final case class HandNotes(
  handNotes: Seq[HandNote.Value]
)

object HandNotes extends Element.WithToXml[HandNotes]("handNotes") {
  def apply(): HandNotes = new HandNotes(
    handNotes = Seq.empty
  )

  override def parser: Parser[HandNotes] = for {
    handNotes <- HandNote.parsable.all
  } yield new HandNotes(
    handNotes
  )

  override protected val antiparser: Antiparser[HandNotes] = Tei.concat(
    // TODO why do I need [HandNotes] in compose() here?
    HandNote.parsable.toXmlSeq.compose[HandNotes](_.handNotes)
  )
}
