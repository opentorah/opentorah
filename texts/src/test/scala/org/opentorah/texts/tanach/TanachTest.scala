package org.opentorah.texts.tanach

import org.opentorah.texts.TestBase
import org.podval.metadata.Language
import org.podval.store.Path

final class TanachTest extends TestBase(Tanach):
  // TODO put into common base class in tanach package if I am going to make - say - separate PsalmsTest:
  def checkChapterLength(path: String, length: Int): Unit =
    resolve(path).lastAs[Chapter].length shouldBe length
  def checkVerseNumber(path: String, number: Int): Unit =
    resolve(path).lastAs[Verse].number shouldBe number

  "Tanach" should "load" in:
    Tanach.Chumash.Genesis.chapters.length(17) shouldBe 27
    Parsha.Vayikra.aliyot.spans(2).span.from.verse shouldBe 10

  it should "load Part names" in:
    checkName("/", "Танах")

  it should "contain /book/Genesis" in:
    checkName("/book/Genesis", "Бытие")

  it should "contain /book/Бытие" in:
    checkName("/book/Бытие", "Genesis")

  it should "contain /book/Genesis/chapter/3" in:
    checkChapterLength("/book/Genesis/chapter/3", 24)

  it should "contain /book/Exodus/chapter/1" in:
    checkChapterLength("/book/Exodus/chapter/1", 22)

  it should "contain /book/Exodus/chapter/1/verse/3" in:
    checkVerseNumber("/book/Exodus/chapter/1/verse/3", 3)

  it should "contain /book/Exodus/chapter/א/verse/ג" in:
    checkVerseNumber("/book/Exodus/chapter/א/verse/ג", 3)

  it should "contain /book/Бытие/parsha/Noach" in:
    checkName("/book/Бытие/parsha/Noach", "Ноах")

  it should "contain /book/Бытие/parsha/Бытие/chapter/1/verse/1" in:
    checkVerseNumber("/book/Бытие/parsha/Бытие/chapter/1/verse/1", 1)

  it should "contain /part/Prophets" in:
    checkName("/part/Prophets", "Пророки")

  it should "contain /part/Prophets/book/Joshua" in:
    checkName("/part/Prophets/book/Joshua", "Ехошуа")

  it should "contain /part/Prophets/book/Judges" in:
    checkName("/part/Prophets/book/Judges", "Шофтим")

  it should "contain /part/Writings" in:
    checkName("/part/Writings", "כתובים")

  it should "contain /part/Prophets/part/Early Prophets/book/Joshua" in:
    checkName("/part/Prophets/part/Early Prophets/book/Joshua", "Ехошуа")

  it should "contain /Chumash/book/Genesis" in:
    checkName("/Chumash/book/Genesis", "Бытие")

  it should "contain /Хумаш/book/Genesis" in:
    checkName("/Хумаш/book/Genesis", "Бытие")

  it should "contain /Psalms/chapter/119/verse/150" in:
    checkName("/Psalms/chapter/119/verse/150", "150")

  it should "contain /Псалмы/chapter/119/verse/150" in:
    checkName("/Псалмы/chapter/119/verse/150", "150")

  it should "expand aliases to the canonical English URL" in:
    resolve("/Chumash").toUrl shouldBe "/part/Chumash"
    resolve("/Psalms").toUrl shouldBe "/book/Psalms"

  it should "contain /book/Psalms/chapter/119/verse/150" in:
    checkName("/book/Psalms/chapter/119/verse/150", "150")

  it should "contain /book/Psalms/book/5/chapter/119/verse/150" in:
    checkName("/book/Psalms/book/5/chapter/119/verse/150", "150")

  it should "contain /book/Psalms/day/26/chapter/119/verse/150" in:
    checkName("/book/Psalms/day/26/chapter/119/verse/150", "150")

  it should "contain /book/Psalms/day of the week/6/chapter/119/verse/150" in:
    checkName("/book/Psalms/day of the week/6/chapter/119/verse/150", "150")

  it should "contain /book/Psalms/day of the week/Friday/chapter/119/verse/150" in:
    checkName("/book/Psalms/day of the week/Friday/chapter/119/verse/150", "150")
    val friday = resolve("/book/Psalms/day of the week/Friday/chapter/119/verse/150")
    val numbered = resolve("/book/Psalms/day of the week/6/chapter/119/verse/150")
    friday.structureNames should contain("Friday")
    numbered.toUrl shouldBe friday.toUrl

  it should "walk each book on every axis and skip aliases" in:
    val paths: Seq[Path] = Tanach.getPaths(
      include = _.isInstanceOf[TanachBook],
      stop = _.isInstanceOf[TanachBook]
    )
    val names: Seq[Seq[String]] = paths.map(_.structureNames)
    def bookName(book: TanachBook): String = book.names.doFind(Language.English.toSpec).name

    names should contain(Seq("book", bookName(Tanach.Book.Genesis)))
    names should contain(Seq("part", "Chumash", "book", bookName(Tanach.Book.Genesis)))
    resolve("/book/Genesis").last should be theSameInstanceAs resolve("/part/Chumash/book/Genesis").last

    names should contain(Seq("book", bookName(Tanach.Book.Joshua)))
    names should contain(Seq("part", "Prophets", "book", bookName(Tanach.Book.Joshua)))
    names should contain(Seq("part", "Prophets", "part", "Early Prophets", "book", bookName(Tanach.Book.Joshua)))

    names should contain(Seq("book", bookName(Tanach.Psalms)))
    names should contain(Seq("part", "Writings", "book", bookName(Tanach.Psalms)))
    names should not contain Seq("Psalms")
    names should not contain Seq("Chumash")

    Tanach.Book.valuesSeq.foreach: book =>
      names should contain(Seq("book", bookName(book)))
    paths.length should be > Tanach.Book.valuesSeq.length
    paths.foreach: path =>
      path.last shouldBe a[TanachBook]
      Tanach.resolve(path.toUrl).last should be theSameInstanceAs path.last

  it should "return the same numbered store on a second resolve" in:
    resolveLast("/book/Genesis/chapter/1/verse/1") should be theSameInstanceAs
      resolveLast("/book/Genesis/chapter/1/verse/1")

  it should "not equate chapters of different books" in:
    resolveLast("/book/Genesis/chapter/1") should not equal resolveLast("/book/Exodus/chapter/1")

  it should "skip a unique By hop" in:
    resolve("/Genesis").toUrl shouldBe resolve("/book/Genesis").toUrl
    resolve("/Genesis").last should be theSameInstanceAs resolve("/book/Genesis").last
    resolve("/book/Genesis/1/1").toUrl shouldBe resolve("/book/Genesis/chapter/1/verse/1").toUrl
    resolve("/book/Genesis/1/1").last should be theSameInstanceAs
      resolve("/book/Genesis/chapter/1/verse/1").last
    resolve("/Бытие/1/1").last should be theSameInstanceAs
      resolve("/book/Genesis/chapter/1/verse/1").last

  it should "require the hop when several By axes match" in:
    intercept[IllegalArgumentException] { resolve("/book/Psalms/1") }
    checkVerseNumber("/book/Psalms/chapter/1/verse/1", 1)

