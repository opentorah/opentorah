package org.opentorah.tei

import org.opentorah.xml.{Unparser, Element, Parsable, Parser}

final class HandNotes(
  val handNotes: Seq[HandNote.Value]
)

object HandNotes extends Element[HandNotes]("handNotes"):
  def empty: HandNotes = HandNotes(
    handNotes = Seq.empty
  )

  override def contentParsable: Parsable[HandNotes] = new Parsable[HandNotes]:
    override def parser: Parser[HandNotes] = for
      handNotes: Seq[HandNote.Value] <- HandNote.element.seq()
    yield HandNotes(
      handNotes
    )

    override val unparser: Unparser[HandNotes] = Tei.concat(
      HandNote.element.seq(_.handNotes)
    )
