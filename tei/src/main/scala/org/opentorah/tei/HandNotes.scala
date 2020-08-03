package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parser}

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

  override protected val antiparser: Antiparser[HandNotes] = Antiparser(
    // TODO why do I need [] in premap() here?
    HandNote.parsable.elementAntiparserSeq.premap[HandNotes](_.handNotes)
  )
}
