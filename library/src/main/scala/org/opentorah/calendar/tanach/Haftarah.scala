package org.opentorah.calendar.tanach

import org.opentorah.metadata.{LanguageSpec, Metadata, Names, WithNumber}
import org.opentorah.util.Collections
import org.opentorah.xml.{Attribute, Element, From, Parser}
import org.opentorah.judaica.tanach.{Custom, Parsha, Tanach, WithBookSpans}
import zio.ZIO
import scala.xml.Elem

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
      elementName = "week",
      parser = Names.withDefaultName(parser(true))
    )

    val result: Seq[(Parsha, (String, Customs))] = Metadata.bind(
      keys = Parsha.values,
      metadatas,
      (metadata: (String, Customs), name: String) => metadata._1 == name
    )

    Collections.mapValues(result.toMap)(_._2)
  }

  // TODO rework to avoid peeking at the name of the next nested element
  def parser(full: Boolean): Parser[Customs] = for {
    span <- spanParser
    hasParts <- Element.nextNameIs("part")
    parts <- if (!hasParts) ZIO.none else partsParser(span).map(Some(_))
    hasCustom <- Element.nextNameIs("custom")
    customs <- customParsable(span).all.map(customs => Custom.Of(customs, full = false))
  } yield {
    val common: Option[Haftarah] = if (!hasParts && !hasCustom) Some(oneSpan(span)) else parts

    val result = common.fold(customs.customs) { common =>
      require(customs.find(Custom.Common).isEmpty)
      customs.customs.updated(Custom.Common, common)
    }

    new Custom.Of(result, full = full)
  }

  private def customParsable(span: BookSpanParsed): Element[(Set[Custom], Haftarah)] =
    new Element[(Set[Custom], Haftarah)](
      "custom",
      parser = customParser(span)
    ) {
      override def toXml(value: (Set[Custom], Haftarah)): Elem = ??? // TODO
    }

  private def oneSpan(span: BookSpanParsed): Haftarah = Haftarah(Seq(span.resolve))

  private def customParser(ancestorSpan: BookSpanParsed): Parser[(Set[Custom], Haftarah)] = for {
    n <- Attribute("n").required
    bookSpanParsed <- spanParser.map(_.inheritFrom(ancestorSpan))
    hasParts <- Element.nextNameIs("part")
    result <- if (!hasParts) ZIO.succeed[Haftarah](oneSpan(bookSpanParsed)) else partsParser(bookSpanParsed)
  } yield Custom.parse(n) -> result

  private def partsParser(ancestorSpan: BookSpanParsed): Parser[Haftarah] = for {
    parts <- partParsable(ancestorSpan).all
    _ <- WithNumber.checkConsecutiveNg(parts, "part")
    _ <- Parser.check(parts.length > 1, "too short")
  } yield Haftarah(WithNumber.dropNumbers(parts))

  private def partParsable(ancestorSpan: BookSpanParsed): Element[WithNumber[BookSpan]] =
    new Element[WithNumber[BookSpan]](
    elementName = "part",
    parser = WithNumber.parse(spanParser.map(_.inheritFrom(ancestorSpan).resolve))
  ) {
      override def toXml(value: WithNumber[Haftarah.BookSpan]): Elem = ??? // TODO
    }
}
