package org.opentorah.html

import org.opentorah.xml.Xml
import zio.{Has, URIO, ZLayer}

// TODO add pre-existing ids as a set and take it into account when getting a new id (including for notes)
// TODO add nested section ids
object endnotes {

  type EndNotes = Has[EndNotes.Service]

  object EndNotes {

    trait Service {
      def addEndNote(id: Option[String], content: Xml.Nodes): Xml.Element

      def getEndNotes: Seq[Xml.Element]
    }

    def empty: ZLayer[Any, Nothing, EndNotes] = ZLayer.succeed(new Service {
      private var endNotes: Seq[EndNote] = Seq.empty

      override def getEndNotes: Seq[Xml.Element] = endNotes.map(_.body)

      // TODO get two ids, one for the actual content at the end
      override def addEndNote(id: Option[String], content: Xml.Nodes): Xml.Element = {
        val note: EndNote = new EndNote(
          number = endNotes.length + 1,
          id,
          content
        )

        endNotes = endNotes :+ note

        note.link
      }
    })
  }

  def addEndNote(id: Option[String], content: Xml.Nodes): URIO[EndNotes, Xml.Element] =
    URIO.access(_.get.addEndNote(id, content))

  def getEndNotes: URIO[EndNotes, Seq[Xml.Element]] =
    URIO.access(_.get.getEndNotes)
}
