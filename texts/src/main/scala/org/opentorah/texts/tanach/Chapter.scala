package org.opentorah.texts.tanach

import org.opentorah.metadata.WithNumber
import org.opentorah.store.{By, NumberedStore, NumberedStores, Pure, Selector, Store}
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser}

abstract class Chapter(override val number: Int, from: Int, to: Int) extends NumberedStore, Pure[?]:
  def length: Int = to - from + 1
  override def storesPure: Seq[By[?]] = Seq(Chapter.ByVerse(from, to))

object Chapter extends Element[WithNumber[Int]]("chapter"):

  final class ByVerse(override val minNumber: Int, override val maxNumber: Int) extends
    By.WithSelector[Verse]("verse"),
    NumberedStores[Verse]:
    override protected def createNumberedStore(number: Int): Verse = new Verse(number):
      override def oneOf: NumberedStores[Verse] = ByVerse.this

  private val lengthAttribute: Attribute.Required[Int] = Attribute.PositiveIntAttribute("length").required

  override def contentParsable: Parsable[WithNumber[Int]] = new Parsable[WithNumber[Int]]:
    override def parser: Parser[WithNumber[Int]] = WithNumber.parse(lengthAttribute())
    override def unparser: Unparser[WithNumber[Int]] = ???
