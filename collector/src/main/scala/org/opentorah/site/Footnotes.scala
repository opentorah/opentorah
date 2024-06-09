package org.opentorah.site

import org.opentorah.util.Effects
import org.opentorah.xml.Xml
import zio.{URIO, ZIO, ZLayer}

class Footnotes:
  private var nextIdNumber: Int = 1

  private def takeNextIdNumber: Int =
    val result: Int = nextIdNumber
    nextIdNumber += 1
    result

  // Stack of footnote lists for each of the nested containers; head is the current one.
  private var footnotes: Seq[Seq[Xml.Element]] = Seq.empty

  def isEmpty: Boolean = footnotes.isEmpty

  // There may be multiple levels of footnotes, so emptying the head of the stack is separate from popping it.
  def push(): Unit = footnotes = Seq.empty +: footnotes

  def pop(): Unit = footnotes = footnotes.tail

  private def getNextNumber: Int = footnotes.head.length + 1

  def add(footnote: Xml.Element): Unit =
    footnotes = (footnotes.head :+ footnote) +: footnotes.tail

  def get: Seq[Xml.Element] =
    val result: Seq[Xml.Element] = footnotes.head
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

  def get: URIO[Footnotes, Seq[Xml.Element]] = ZIO.environmentWith(_.get.get)

  def empty: ZLayer[Any, Nothing, Footnotes] = ZLayer.succeed(new Footnotes)

  def footnote(element: Xml.Element): ZIO[Footnotes, Effects.Error, Xml.Element] = for
  // TODO get two ids, one for the actual content at the end
    idNumber: Int <- ZIO.environmentWith[Footnotes](_.get.takeNextIdNumber)
    id: Option[String] <- Xml.idAttribute.optional.get(element)
    srcId: String = id.getOrElse(s"footnote_src_$idNumber")
    contentId: String = s"footnote_$idNumber"
    number: Int <- ZIO.environmentWith[Footnotes](_.get.getNextNumber)
    symbol: String = number.toString
    footnote: Xml.Element = TeiToHtml.footnote(contentId, srcId, symbol, Xml.getChildren(element))
    _ <- ZIO.environmentWith[Footnotes](_.get.add(footnote))
  yield
    TeiToHtml.footnoteRef(contentId, srcId, symbol)
