package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.{Attributes, XML}
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan
import org.podval.judaica.metadata.tanach.{Aliyot, Span}

import scala.xml.Elem

trait TorahReadings {
  type Torah = TorahReadings.Torah

  protected final def parseTorahForShabbosAndWeekday(drop1: Int, drop2: Int, element: Elem): (Torah, Torah) =
    parseTorahForShabbosAndWeekday(Set(drop1, drop2), element)

  protected final def parseTorahForShabbosAndWeekday(toDrop: Set[Int], element: Elem): (Torah, Torah) = {
    val shabbosAliyot: Torah = parseTorah(element)
    (shabbosAliyot, drop(shabbosAliyot, toDrop))
  }

  protected final def parseTorah(element: Elem): Torah = {
    val (attributes: Attributes, elements: Seq[Elem]) = XML.open(element, "torah")
    val bookSpan: ChumashSpan.BookSpan = ChumashSpan.parse(attributes).resolve
    attributes.close()
    val fromChapter: Int = bookSpan.span.from.chapter
    val result: Seq[Span.Numbered] =
      elements.map(element => XML.parseEmpty(element, "aliyah", parseNumbered(fromChapter)))
    Aliyot.parse(bookSpan, result, number = None)
  }

  private def parseNumbered(fromChapter: Int)(attributes: Attributes): Span.Numbered = {
    val span: Span.SemiResolved = Span.parse(attributes).defaultFromChapter(fromChapter).semiResolve
    require(span.to.isEmpty, s"Non-empty to: $span")
    Span.Numbered(
      n = attributes.doGetInt("n"),
      span = span
    )
  }

  protected final def drop(torah: Torah, toDrop: Set[Int]): Torah = {
    def drop(what: Seq[(ChumashSpan.BookSpan, Boolean)]): Seq[ChumashSpan.BookSpan] = what match {
      case (a1, d1) :: (a2, d2) :: tail =>
        if (d2) drop((a1+a2, d1) +: tail)
        else a1 +: drop((a2, d2) +: tail)
      case (a1, d1) :: Nil =>
        Seq(a1)
    }
    val withDrop = torah.zipWithIndex.map { case (a, i) => (a, toDrop.contains(i+1)) }
    require(!withDrop.head._2)
    drop(withDrop)
  }

  protected final def parseMaftir(element: Elem): ChumashSpan.BookSpan =
    XML.parseEmpty(element, "maftir", ChumashSpan.parse).resolve
}

object TorahReadings {
  type Torah = Seq[ChumashSpan.BookSpan]

  def torah7to6(torah: Torah): Torah = torah.take(5) :+ (torah(5)+torah(6))
}
