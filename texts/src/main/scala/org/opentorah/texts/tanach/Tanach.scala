package org.opentorah.texts.tanach

import org.opentorah.metadata.{HasName, HasValues, Named, Names}
import org.opentorah.store.{By, Pure, Store}

object Tanach extends Pure[?]:
  override def names: Names = All.names

  // Part markers
  trait Chumash extends ChumashBook {}
  trait Nach extends NachBook {}
    trait Prophets extends Nach {}
      trait EarlyProphets extends Prophets {}
      trait LateProphets extends Prophets {}
        trait TreiAsar extends LateProphets {}
    trait Writings extends Nach {}
      trait Psalms extends Writings, PsalmsBook {}

  // Books
  enum Book(nameOverride: Option[String] = None) extends TanachBook, HasName(nameOverride), HasName.Enum derives CanEqual:
    case Genesis extends Book, Chumash
    case Exodus extends Book, Chumash
    case Leviticus extends Book, Chumash
    case Numbers extends Book, Chumash
    case Deuteronomy extends Book, Chumash

    case Joshua extends Book, EarlyProphets
    case Judges extends Book, EarlyProphets
    case SamuelI extends Book(nameOverride = Some("I Samuel")), EarlyProphets
    case SamuelII extends Book(nameOverride = Some("II Samuel")), EarlyProphets
    case KingsI extends Book(nameOverride = Some("I Kings")), EarlyProphets
    case KingsII extends Book(nameOverride = Some("II Kings")), EarlyProphets

    case Isaiah extends Book, LateProphets
    case Jeremiah extends Book, LateProphets
    case Ezekiel extends Book, LateProphets

    case Hosea extends Book, TreiAsar
    case Joel extends Book, TreiAsar
    case Amos extends Book, TreiAsar
    case Obadiah extends Book, TreiAsar
    case Jonah extends Book, TreiAsar
    case Micah extends Book, TreiAsar
    case Nahum extends Book, TreiAsar
    case Habakkuk extends Book, TreiAsar
    case Zephaniah extends Book, TreiAsar
    case Haggai extends Book, TreiAsar
    case Zechariah extends Book, TreiAsar
    case Malachi extends Book, TreiAsar

    case Psalms extends Book, Psalms
    case Proverbs extends Book, Writings
    case Job extends Book, Writings
    case SongOfSongs extends Book(nameOverride = Some("Song of Songs")), Writings
    case Ruth extends Book, Writings
    case Lamentations extends Book, Writings
    case Ecclesiastes extends Book, Writings
    case Esther extends Book, Writings
    case Daniel extends Book, Writings
    case Ezra extends Book, Writings
    case Nehemiah extends Book, Writings
    case ChroniclesI extends Book(nameOverride = Some("I Chronicles")), Writings
    case ChroniclesII extends Book(nameOverride = Some("II Chronicles")), Writings

  object Book extends HasValues.FindByName[Book]:
    override def valuesSeq: Seq[Book] = values.toIndexedSeq

  // Parts
  sealed class Part[T <: TanachBook](
    clazz: Class[T],
    nameOverride: Option[String] = None,
  ) extends Named.ByLoader[Part[?]](loader = Part, nameOverride), HasName.NonEnum, Pure[?]:
    protected def byBook: By[TanachBook] =
      new By.WithSelector[TanachBook](selectorName = "book")
        with Pure.With[TanachBook](storesPure = Book.valuesSeq.filter(clazz.isInstance))

    override def storesPure: Seq[Store] = Seq(byBook)

    final def forName(name: String): T = Book.getForName(name).asInstanceOf[T]

  private object All extends Part(classOf[TanachBook], nameOverride = Some("Tanach"))

  object Chumash extends Part(classOf[Chumash]):
    def Genesis: Chumash = Book.Genesis
    def Exodus: Chumash = Book.Exodus
    def Leviticus: Chumash = Book.Leviticus
    def Numbers: Chumash = Book.Numbers
    def Deuteronomy: Chumash = Book.Deuteronomy

  object Nach extends Part(classOf[Nach])

  object Prophets extends Part(classOf[Prophets]):
    override def storesPure: Seq[Store] = Seq(
      byBook,
      new By.WithSelector[Part[?]](selectorName = "part")
        with Pure.With[Part[?]](storesPure = Seq(EarlyProphets, LateProphets,TreiAsar))
    )

  object EarlyProphets extends Part(classOf[EarlyProphets], nameOverride = Some("Early Prophets"))
  object LateProphets extends Part(classOf[LateProphets], nameOverride = Some("Late Prophets"))
  object TreiAsar extends Part(classOf[TreiAsar], nameOverride = Some("Trei Asar"))
  object Writings extends Part(classOf[Writings])

  def Psalms: Psalms = Book.Psalms

  private object Part extends Names.Loader[Part[?]]:
    override def valuesSeq: Seq[Part[?]] = Seq(All, Chumash, Nach, Prophets, EarlyProphets, LateProphets, TreiAsar, Writings)

  // Stores
  // TODO when I have aliases, install them for Chumash and Psalm here (and higher?)
  override def storesPure: Seq[Store] = Seq(
    new By.WithSelector[TanachBook](selectorName = "book")
      with Pure.With[TanachBook](storesPure = Book.valuesSeq),
    new By.WithSelector[Part[?]](selectorName = "part")
      with Pure.With[Part[?]](storesPure = Seq(Chumash, Prophets, Writings))
  )
