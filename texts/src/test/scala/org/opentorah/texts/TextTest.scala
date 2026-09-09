package org.opentorah.texts

final class TextTest extends TestBase(Text):

  "Text" should "contain /Tanach" in:
    checkName("/Tanach", "Танах")

  it should "contain /Mishneh Torah" in:
    checkName("/Mishneh Torah", "Mishneh Torah")

  it should "contain /Sefer Hamitzvos" in:
    checkName("/Sefer Hamitzvos", "Sefer Hamitzvos")

  it should "resolve a Mishneh Torah chapter from the root" in:
    resolve("/Mishneh Torah/Sefer Madda/Yesodei haTorah/1").lastAs[rambam.MishnehTorah.NumberedChapter].number shouldBe 1
