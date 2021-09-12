package org.opentorah.texts.tanach

import org.opentorah.metadata.WithNumber
import org.opentorah.store.{By, Selector, Store, Stores}
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser}

final class Chapter(override val number: Int, val length: Int) extends Store.Numbered, Store.NonTerminal, Stores.Pure:
  final class VersesBy extends By, Stores.Numbered[VerseStore]:
    override def selector: Selector = Selector.byName("verse")
    override def length: Int = Chapter.this.length
    override protected def createNumberedStore(number: Int): VerseStore = VerseStore(number)

  override def storesPure: Seq[VersesBy] = Seq(new VersesBy)

object Chapter extends Element[WithNumber[Int]]("chapter"):

  private val lengthAttribute: Attribute.Required[Int] = Attribute.PositiveIntAttribute("length").required

  override def contentParsable: Parsable[WithNumber[Int]] = new Parsable[WithNumber[Int]]:
    override def parser: Parser[WithNumber[Int]] = WithNumber.parse(lengthAttribute())
    override def unparser: Unparser[WithNumber[Int]] = ???
