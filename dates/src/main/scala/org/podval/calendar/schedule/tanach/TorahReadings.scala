package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.{Attributes, XML}
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan
import org.podval.judaica.metadata.tanach.{Aliyot, Span}

import scala.xml.Elem

trait TorahReadings {
  type Torah = Seq[ChumashSpan.BookSpan]

  protected final def parseTorahForShabbosAndWeekday(element: Elem): (Torah, Torah) = {
    val (attributes: Attributes, elements: Seq[Elem]) = XML.open(element, "torah")
    val bookSpan: ChumashSpan.BookSpan = ChumashSpan.parse(attributes).resolve
    attributes.close()

    val fromChapter: Int = bookSpan.span.from.chapter
    def parseNumberedWithShabbos(attributes: Attributes): (Span.Numbered, Boolean) =
      (parseNumbered(fromChapter)(attributes), attributes.doGetBoolean("shabbos"))

    val result: Seq[(Span.Numbered, Boolean)] =
      elements.map(element => XML.parseEmpty(element, "aliyah", parseNumberedWithShabbos))
    val shabbosAliyot: Torah = Aliyot.parse(bookSpan, result.map(_._1), number = Some(7))
    val toDrop: Set[Int] = result.filter(_._2).map(_._1.n).toSet
    (shabbosAliyot, drop(shabbosAliyot, toDrop))
  }

  protected final def parseTorahForShabbosAndWeekday(toDrop: Set[Int], element: Elem): (Torah, Torah) = {
    val shabbosAliyot: Torah = parseTorah(element)
    (shabbosAliyot, drop(shabbosAliyot, toDrop))
  }

  protected final def parseTorah(element: Elem): Torah = {
    val (attributes: Attributes, elements: Seq[Elem]) = XML.open(element, "torah")
    val bookSpan: ChumashSpan.BookSpan = ChumashSpan.parse(attributes).resolve
    attributes.close()
    val fromChapter: Int = bookSpan.span.from.chapter
    val result = elements.map(element => XML.parseEmpty(element, "aliyah", parseNumbered(fromChapter)))
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

  private def drop(torah: Torah, toDrop: Set[Int]): Torah = {
    require(!toDrop.contains(1))
    // TODO incorrect!!!
    torah.zipWithIndex.filterNot { case (_, index: Int) => toDrop.contains(index + 1) }.map(_._1)
  }

  protected final def parseMaftir(element: Elem): ChumashSpan.BookSpan =
    XML.parseEmpty(element, "maftir", ChumashSpan.parse).resolve
}
