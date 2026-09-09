package org.opentorah.texts.rambam

import org.opentorah.texts.TestBase
import org.podval.store.Path

final class MishnehTorahTest extends TestBase(MishnehTorah):

  "Mishneh Torah" should "load books" in:
    MishnehTorah.books.length shouldBe 15

  it should "contain named books and parts" in:
    checkName("/book/The Book of Knowledge", "Sefer Madda")
    checkName("/book/Sefer Madda/part/Yesodei haTorah", "foundations of Torah")

  it should "resolve a numbered chapter" in:
    resolve("/book/Sefer Madda/part/Yesodei haTorah/chapter/1").lastAs[MishnehTorah.NumberedChapter].number shouldBe 1

  it should "skip unique By hops" in:
    val typed: Path = resolve("/book/Sefer Madda/part/Yesodei haTorah/chapter/1")
    resolve("/Sefer Madda/Yesodei haTorah/1").toUrl shouldBe typed.toUrl
    resolve("/Sefer Madda/Yesodei haTorah/1").last should be theSameInstanceAs typed.last

  it should "resolve a named chapter" in:
    checkName(
      "/book/Introduction by Rambam/part/Introduction by Rambam/chapter/Introduction: Then etc. All the mitzvos... from Hashem the G-d of Israel",
      "Introduction: Then etc. All the mitzvos... from Hashem the G-d of Israel"
    )
