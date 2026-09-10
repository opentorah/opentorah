package org.opentorah.texts.rambam

import org.opentorah.texts.TestBase

final class SeferHamitzvosLessonsTest extends TestBase(SeferHamitzvosLessons):

  "Sefer Hamitzvos" should "load lessons" in:
    SeferHamitzvosLessons.lessons.length shouldBe 339

  it should "resolve a numbered lesson" in:
    resolve("/lesson/5").lastAs[SeferHamitzvosLessons.Lesson].number shouldBe 5

  it should "resolve a commandment through the lesson and from the root" in:
    val viaLesson = resolve("/lesson/5/positive/1").lastAs[SeferHamitzvosLessons.Positive]
    viaLesson.number shouldBe 1
    val fromRoot = resolve("/positive/1")
    fromRoot.lastAs[SeferHamitzvosLessons.Positive] shouldBe viaLesson
    fromRoot.toUrl should endWith("/positive/1")
    resolve(fromRoot.toUrl).last should be theSameInstanceAs fromRoot.last

  it should "resolve a named lesson part" in:
    checkName("/lesson/2", "2")
    checkName("/lesson/2/The principles", "כללים")
