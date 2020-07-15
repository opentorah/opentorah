package org.opentorah.texts.tanach

import org.opentorah.metadata.{LanguageSpec, Metadata, Names, WithNumber}
import org.opentorah.util.Collections
import org.opentorah.xml.{Attribute, Element, From, Parser}
import zio.ZIO

final case class Haftarah private(override val spans: Seq[Haftarah.BookSpan])
  extends Haftarah.Spans(spans)

object Haftarah extends WithBookSpans[Tanach.ProphetsBook] {
  override type Many = Haftarah

  override def apply(spans: Seq[BookSpan]): Haftarah = new Haftarah(spans)

  def toLanguageString(spans: Seq[BookSpan])(implicit spec: LanguageSpec): String = {
    Collections.group(spans, (span: BookSpan) => span.book)
      .map { bookSpans =>
        bookSpans.head.book.toLanguageString + " " + bookSpans.map(_.span.toLanguageString).mkString(", ")
      }.mkString("; ")
  }

  override protected def getBook(name: String): Tanach.ProphetsBook = Tanach.getProhetForName(name)

  final def forParsha(parsha: Parsha): Customs = haftarah(parsha).map(_.from(parsha))

  private lazy val haftarah: Map[Parsha, Customs] = {
    val metadatas: Seq[(String, Customs)] = Metadata.load(
      from = From.resource(this),
      elementParsable = new Element[(String, Customs)]("week") {
        override protected def parser: Parser[(String, Customs)] = Names.withDefaultName(parserX(true))
      }
    )

    val result: Seq[(Parsha, (String, Customs))] = Metadata.bind(
      keys = Parsha.values,
      metadatas,
      (metadata: (String, Customs), name: String) => metadata._1 == name
    )

    Collections.mapValues(result.toMap)(_._2)
  }

  def haftarahParsable(full: Boolean): Element[Customs] = new Element[Customs]("haftarah") {
    override protected def parser: Parser[Customs] = parserX(full)
  }

  private def parserX(full: Boolean): Parser[Customs] = for {
    span <- spanParser
    parts <- partParsable(span).all
    parts <- if (parts.isEmpty) ZIO.none else partsParser(parts).map(Some(_))
    customsElements <- new Element[(Set[Custom], Haftarah)]("custom") {
      override protected def parser: Parser[(Set[Custom], Haftarah)] = customParser(span)
    }.all
  } yield {
    val customs: Custom.Of[Haftarah] = Custom.Of(customsElements, full = false)
    val common: Option[Haftarah] = if (parts.isEmpty && customsElements.isEmpty) Some(oneSpan(span)) else parts

    val result: Map[Custom, Haftarah] = common.fold(customs.customs) { common =>
      require(customs.find(Custom.Common).isEmpty)
      customs.customs.updated(Custom.Common, common)
    }

    new Custom.Of(result, full = full)
  }

  private def oneSpan(span: BookSpanParsed): Haftarah = Haftarah(Seq(span.resolve))

  private def customParser(ancestorSpan: BookSpanParsed): Parser[(Set[Custom], Haftarah)] = for {
    n <- Attribute("n").required
    bookSpanParsed <- spanParser.map(_.inheritFrom(ancestorSpan))
    parts <- partParsable(bookSpanParsed).all
    result <- if (parts.isEmpty) ZIO.succeed[Haftarah](oneSpan(bookSpanParsed)) else partsParser(parts)
  } yield Custom.parse(n) -> result

  private def partParsable(ancestorSpan: BookSpanParsed): Element[WithNumber[BookSpan]] = new Element[WithNumber[BookSpan]]("part") {
    override protected def parser: Parser[WithNumber[BookSpan]] = WithNumber.parse(spanParser.map(_.inheritFrom(ancestorSpan).resolve))
  }

  private def partsParser(parts: Seq[WithNumber[BookSpan]]): Parser[Haftarah] = for {
    _ <- WithNumber.checkConsecutive(parts, "part")
    _ <- Parser.check(parts.length > 1, "too short")
  } yield Haftarah(WithNumber.dropNumbers(parts))
}
