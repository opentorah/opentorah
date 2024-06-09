package org.opentorah.site

import org.opentorah.util.Effects
import org.opentorah.xml.{ScalaXml, Xml}
import zio.{URIO, ZIO, ZLayer}

class Footnotes:
  private var nextIdNumber: Int = 1

  private def takeNextIdNumber: Int =
    val result: Int = nextIdNumber
    nextIdNumber += 1
    result

  // Stack of footnote lists for each of the nested containers; head is the current one.
  private var footnotes: Seq[Seq[ScalaXml.Element]] = Seq.empty

  def isEmpty: Boolean = footnotes.isEmpty

  // There may be multiple levels of footnotes, so emptying the head of the stack is separate from popping it.
  def push(): Unit = footnotes = Seq.empty +: footnotes

  def pop(): Unit = footnotes = footnotes.tail

  private def getNextNumber: Int = footnotes.head.length + 1

  def add(footnote: ScalaXml.Element): Unit =
    footnotes = (footnotes.head :+ footnote) +: footnotes.tail

  def get: Seq[ScalaXml.Element] =
    val result: Seq[ScalaXml.Element] = footnotes.head
    footnotes = Seq.empty +: footnotes.tail
    result

// TODO add pre-existing ids as a set and take it into account when getting a new id (including for notes)
// TODO add nested section ids
// TODO use ZIO.service()
// TODO introduce footnote types (sections; differentiated by the reference symbol)
object Footnotes:
  def isEmpty: URIO[Footnotes, Boolean] = ZIO.environmentWith(_.get.isEmpty)
  def push: URIO[Footnotes, Unit] = ZIO.environmentWith(_.get.push())
  def pop: URIO[Footnotes, Unit] = ZIO.environmentWith(_.get.pop())

  def get: URIO[Footnotes, Seq[ScalaXml.Element]] = ZIO.environmentWith(_.get.get)

  def empty: ZLayer[Any, Nothing, Footnotes] = ZLayer.succeed(new Footnotes)

  def footnote(element: ScalaXml.Element): ZIO[Footnotes, Effects.Error, ScalaXml.Element] = for
  // TODO get two ids, one for the actual content at the end
    idNumber: Int <- ZIO.environmentWith[Footnotes](_.get.takeNextIdNumber)
    id: Option[String] <- Xml.idAttribute.optional.get(ScalaXml)(element)
    srcId: String = id.getOrElse(s"footnote_src_$idNumber")
    contentId: String = s"footnote_$idNumber"
    number: Int <- ZIO.environmentWith[Footnotes](_.get.getNextNumber)
    symbol: String = number.toString
    footnote: ScalaXml.Element = TeiToHtml.footnote(contentId, srcId, symbol, ScalaXml.getChildren(element))
    _ <- ZIO.environmentWith[Footnotes](_.get.add(footnote))
  yield
    TeiToHtml.footnoteRef(contentId, srcId, symbol)
