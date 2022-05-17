package org.opentorah.html

import org.opentorah.xml.ScalaXml
import zio.{URIO, ZIO, ZLayer}

class Footnotes:
  private var nextIdNumber: Int = 1

  def takeNextIdNumber: Int =
    val result: Int = nextIdNumber
    nextIdNumber += 1
    result

  // Stack of footnote lists for each of the nested containers; head is the current one.
  private var footnotes: Seq[Seq[ScalaXml.Element]] = Seq.empty

  def isEmpty: Boolean = footnotes.isEmpty

  // There may be multiple levels of footnotes, so emptying the head of the stack is separate from popping it.
  def push(): Unit = footnotes = Seq.empty +: footnotes

  def pop(): Unit = footnotes = footnotes.tail

  def getNextNumber: Int = footnotes.head.length + 1

  def add(footnote: ScalaXml.Element): Unit =
    footnotes = (footnotes.head :+ footnote) +: footnotes.tail

  def get: Seq[ScalaXml.Element] =
    val result: Seq[ScalaXml.Element] = footnotes.head
    footnotes = Seq.empty +: footnotes.tail
    result

// TODO add pre-existing ids as a set and take it into account when getting a new id (including for notes)
// TODO add nested section ids
// TODO use ZIO.service()
object Footnotes:
  def takeNextIdNumber: URIO[Footnotes, Int] = ZIO.environmentWith(_.get.takeNextIdNumber)
  def isEmpty: URIO[Footnotes, Boolean] = ZIO.environmentWith(_.get.isEmpty)
  def push: URIO[Footnotes, Unit] = ZIO.environmentWith(_.get.push())
  def pop: URIO[Footnotes, Unit] = ZIO.environmentWith(_.get.pop())
  def getNextNumber: URIO[Footnotes, Int] = ZIO.environmentWith(_.get.getNextNumber)
  def add(footnote: ScalaXml.Element): URIO[Footnotes, Unit] = ZIO.environmentWith(_.get.add(footnote))
  def get: URIO[Footnotes, Seq[ScalaXml.Element]] = ZIO.environmentWith(_.get.get)

  def empty: ZLayer[Any, Nothing, Footnotes] = ZLayer.succeed(new Footnotes)
