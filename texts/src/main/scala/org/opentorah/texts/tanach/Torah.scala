package org.opentorah.texts.tanach

import org.opentorah.metadata.Named
import org.opentorah.util.Effects
import org.opentorah.xml.Parser
import org.podval.xml.XmlAst
import Tanach.Chumash

// Other than on Simchas Torah, aliyot are from the same book.
// TODO de-case - and figure out why object Torah's creation becomes impossible if 'case' is removed here...
final case class Torah(override val spans: Seq[Torah.BookSpan]) extends Torah.Spans(spans) derives CanEqual:
  override def equals(other: Any): Boolean = other match
    case that: Torah => this.spans == that.spans
    case _ => false

  def drop(toDrop: Set[Int]): Torah =
    def drop(what: Seq[(Torah.Aliyah, Boolean)]): Seq[Torah.Aliyah] = what match
      case (a1, d1) :: (a2, d2) :: tail =>
        if d2
        then drop((a1+a2, d1) +: tail)
        else a1 +: drop((a2, d2) +: tail)
      case (a1, _) :: Nil =>
        Seq(a1)

    val withDrop = spans.zipWithIndex.map((a, i) => (a, toDrop.contains(i+1)))
    require(!withDrop.head._2)
    Torah(drop(withDrop))

  def fromWithNumbers(source: Named): Torah = Torah(
    spans.zipWithIndex.map((aliyah, index) =>
      aliyah.from(source.andNumber(number = index + 1))
    )
  )

object Torah extends WithBookSpans[Chumash]:
  protected override type Many = Torah

  type Fragment = BookSpan

  type Aliyah = BookSpan

  type Maftir = BookSpan

  override protected def getBook(name: String): Chumash = Tanach.Chumash.forName(name)

  def aliyot(spans: BookSpan*): Torah = Torah(spans)

  type Numbered = WithNumber[SpanSemiResolved]

  def parseAliyot(
    bookSpan: BookSpan,
    aliyot: Seq[Numbered],
    number: Option[Int]
  ): Parser[Torah] =
    val span: Span = bookSpan.span
    val chapters: Chapters = bookSpan.book.chapters
    val with1: Seq[Numbered] = addImplied1(aliyot, span, chapters)
    for
      _ <- WithNumber.checkNumber(with1, number.getOrElse(with1.length), "span")
      spans: Seq[Span] = SpanSemiResolved.setImpliedTo(WithNumber.dropNumbers(with1), span, chapters)
      _ <- Effects.check(bookSpan.book.chapters.consecutive(spans), s"Non-consecutive: $spans")
    yield Torah(spans.map(inBook(bookSpan.book, _)))

  def decode[E: XmlAst](element: E): Torah =
    XmlDecode.requireName(element, "torah")
    XmlDecode.requireNoOther(element, Set("aliyah"))
    val bookSpan: BookSpan = decodeSpan(element).resolve
    val spans: Seq[Numbered] = XmlDecode.childrenNamed(element, "aliyah").map(el =>
      WithNumber.decode(el, e => SpanParsed.decode(e).defaultFromChapter(bookSpan.span.from.chapter).semiResolve)
    )
    Parser.unsafeRun(parseAliyot(bookSpan, spans, number = None))

  def decodeMaftir[E: XmlAst](element: E): Maftir =
    XmlDecode.requireName(element, "maftir")
    XmlDecode.requireNoOther(element, Set.empty)
    decodeSpan(element).resolve

  def inBook(book: Chumash, span: Span): BookSpan = BookSpan(book, span)

  def processDays(
    book: Chumash,
    days: Custom.Sets[Seq[Numbered]],
    span: Span
  ): Parser[Torah.Customs] =
    val bookSpan = inBook(book, span)
    val with1 = addImplied1(Custom.common(days), span, book.chapters)

    val result: Parser[Custom.Sets[Torah]] = Effects.mapValues(days)((spans: Seq[Numbered]) =>
      parseAliyot(bookSpan, WithNumber.overlay(with1, spans), Some(7))
    )

    result.map(Custom.Of(_, full = true)) // TODO why does Scala 3 require 'full' being supplied?!

  private def addImplied1(
    spans: Seq[Numbered],
    span: Span,
    chapters: Chapters
  ): Seq[Numbered] =
    val first = spans.head
    val implied1: Seq[Numbered] = if first.n == 1 then Seq.empty else Seq(new Numbered(1, SpanSemiResolved(
      span.from,
      Some(chapters.prev(first.what.from).get)
    )))

    implied1 ++ spans
