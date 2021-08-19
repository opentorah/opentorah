package org.opentorah.html

import org.opentorah.xml.ScalaXml
import zio.{Has, URIO, ZLayer}

trait EndNotes {
  def addEndNote(id: Option[String], content: ScalaXml.Nodes): ScalaXml.Element

  def getEndNotes: Seq[ScalaXml.Element]
}

// TODO add pre-existing ids as a set and take it into account when getting a new id (including for notes)
// TODO add nested section ids
object EndNotes {

  private final class EndNote(
    number: Int,
    id: Option[String],
    val content: ScalaXml.Nodes
  ) {
    private def contentId: String = s"_note_$number"

    private def srcId: String = id.getOrElse(s"src_note_$number")

    def link: ScalaXml.Element =
      a().setFragment(contentId).setId(srcId)(element = <sup>{number}</sup>)

    // TODO is HTML namespace here needed?
    def body: ScalaXml.Element =
      <span xmlns={Html.namespace.uri} class="endnote" id={contentId}>
        {a().setFragment(srcId).addClass("endnote-backlink")(number.toString)}
        {content}
      </span>
  }

  def empty: ZLayer[Any, Nothing, Has[EndNotes]] = ZLayer.succeed(new EndNotes {
    private var endNotes: Seq[EndNote] = Seq.empty

    override def getEndNotes: Seq[ScalaXml.Element] = endNotes.map(_.body)

    // TODO get two ids, one for the actual content at the end
    override def addEndNote(id: Option[String], content: ScalaXml.Nodes): ScalaXml.Element = {
      val note: EndNote = new EndNote(
        number = endNotes.length + 1,
        id,
        content
      )

      endNotes = endNotes :+ note

      note.link
    }
  })

  def addEndNote(id: Option[String], content: ScalaXml.Nodes): URIO[Has[EndNotes], ScalaXml.Element] =
    URIO.access(_.get.addEndNote(id, content))

  def getEndNotes: URIO[Has[EndNotes], Seq[ScalaXml.Element]] =
    URIO.access(_.get.getEndNotes)
}
