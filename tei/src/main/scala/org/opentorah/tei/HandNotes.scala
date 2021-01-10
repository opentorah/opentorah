package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parsable, Parser}

final case class HandNotes(
  handNotes: Seq[HandNote.Value]
)

object HandNotes extends Element[HandNotes]("handNotes") {
  def empty: HandNotes = new HandNotes(
    handNotes = Seq.empty
  )

  override def contentParsable: Parsable[HandNotes] = new Parsable[HandNotes] {
    override def parser: Parser[HandNotes] = for {
      handNotes <- HandNote.element.seq()
    } yield new HandNotes(
      handNotes
    )

    override val antiparser: Antiparser[HandNotes] = Tei.concat(
      HandNote.element.seq(_.handNotes)
    )
  }
}
