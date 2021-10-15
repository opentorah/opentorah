package org.opentorah.texts.tanach

import org.opentorah.texts.TestBase

final class TanachTest extends TestBase(Tanach):
  // TODO put into common base class in tanach package if I am going to make - say - separate PsalmsTest:
  def checkChapterLength(path: String, length: Int): Unit =
    doResolveLast(path).asInstanceOf[Chapter].length shouldBe length
  def checkVerseNumber(path: String, number: Int): Unit =
    doResolveLast(path).asInstanceOf[Verse].number shouldBe number

  "Tanach" should "load" in {
    Tanach.Chumash.Genesis.chapters.length(17) shouldBe 27
    Parsha.Vayikra.aliyot.spans(2).span.from.verse shouldBe 10
  }

  it should "load Part names" in {
    checkName("/", "Танах")
  }

  it should "contain /book/Genesis" in {
    checkName("/book/Genesis", "Бытие")
  }

  it should "contain /book/Бытие" in {
    checkName("/book/Бытие", "Genesis")
  }

  it should "contain /book/Genesis/chapter/3" in {
    checkChapterLength("/book/Genesis/chapter/3", 24)
  }

  it should "contain /book/Exodus/chapter/1" in {
    checkChapterLength("/book/Exodus/chapter/1", 22)
  }

  it should "contain /book/Exodus/chapter/1/verse/3" in {
    checkVerseNumber("/book/Exodus/chapter/1/verse/3", 3)
  }

  it should "contain /book/Exodus/chapter/א/verse/ג" in {
    checkVerseNumber("/book/Exodus/chapter/א/verse/ג", 3)
  }

  it should "contain /book/Бытие/parsha/Noach" in {
    checkName("/book/Бытие/parsha/Noach", "Ноах")
  }

  it should "contain /book/Бытие/parsha/Бытие/chapter/1/verse/1" in {
    checkVerseNumber("/book/Бытие/parsha/Бытие/chapter/1/verse/1", 1)
  }

  it should "contain /part/Prophets" in {
    checkName("/part/Prophets", "Пророки")
  }

  it should "contain /part/Prophets/book/Joshua" in {
    checkName("/part/Prophets/book/Joshua", "Ехошуа")
  }

  it should "contain /part/Prophets/part/Early Prophets/book/Joshua" in {
    checkName("/part/Prophets/part/Early Prophets/book/Joshua", "Ехошуа")
  }

  it should "contain /book/Psalms/chapter/119/verse/150" in {
    checkName("/book/Psalms/chapter/119/verse/150", "150")
  }

  it should "contain /book/Psalms/book/5/chapter/119/verse/150" in {
    checkName("/book/Psalms/book/5/chapter/119/verse/150", "150")
  }

  it should "contain /book/Psalms/day/26/chapter/119/verse/150" in {
    checkName("/book/Psalms/day/26/chapter/119/verse/150", "150")
  }

  it should "contain /book/Psalms/day of the week/6/chapter/119/verse/150" in {
    checkName("/book/Psalms/day of the week/6/chapter/119/verse/150", "150")
  }

  it should "contain /book/Psalms/day of the week/Friday/chapter/119/verse/150" in {
    checkName("/book/Psalms/day of the week/Friday/chapter/119/verse/150", "150")
  }