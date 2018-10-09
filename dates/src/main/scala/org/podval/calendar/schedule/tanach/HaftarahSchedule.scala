package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.tanach.BookSpan.ProphetSpan
import org.podval.judaica.metadata.{Language, LanguageSpec}
import org.podval.judaica.metadata.tanach.{Custom, Haftarah, Parsha, Span}

object HaftarahSchedule {
  private def printHaftarahList(custom: Custom, spec: LanguageSpec, full: Boolean): Unit = {
    println(custom.toString(spec))
    for (parsha <- Parsha.values) {
      val haftarah: Haftarah = parsha.haftarah
      val customEffective: Custom = Custom.find(haftarah.customs, custom)
      val spans: Seq[ProphetSpan.BookSpan] = haftarah.customs(customEffective)
      val result: String = ProphetSpan.toString(spans, spec)

      if (customEffective == custom) {
        println(parsha.toString(spec) + ": " + result)
      } else if (full) {
        println(parsha.toString(spec) + " [" + customEffective.toString(spec)  + "]" + ": " + result)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    def printSpans(spans: Seq[Span]): Unit = spans.zipWithIndex.foreach { case (span, index) =>
      println(s"${index+1}: $span")
    }

    printSpans(Parsha.Mattos.daysCombined(Custom.Ashkenaz))

    printHaftarahList(Custom.Shami, LanguageSpec(Language.Hebrew), full = false)
  }
}
