package org.opentorah.texts.tanach

import org.opentorah.metadata.{LanguageSpec, Metadata, Names, WithNumber}
import org.opentorah.util.Collections
import org.opentorah.xml.{Antiparser, Attribute, Element, From, Parsable, Parser}
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

  object Week extends Element[(String, Customs)]("week") {
    private val elementParser = Haftarah.parser(full = true)

    override def contentParsable: Parsable[(String, Customs)] = new Parsable[(String, Customs)] {
      override def parser: Parser[(String, Customs)] = for {
        name <- Names.defaultNameAttribute.required()
        result <- elementParser
      } yield (name, result)

      override def antiparser: Antiparser[(String, Haftarah.Customs)] = ???
    }
  }

  lazy val haftarah: Map[Parsha, Customs] = Collections.mapValues(Metadata.load(
    from = From.resource(this),
    content = Week,
    keys = Parsha.values,
    hasName = (metadata: (String, Customs), name: String) => metadata._1 == name
  ))(_._2)

  def element(full: Boolean): Element[Customs] = new Element[Customs]("haftarah") {
    override def contentParsable: Parsable[Customs] = new Parsable[Customs] {
      override def parser: Parser[Customs] = Haftarah.parser(full)
      override def antiparser: Antiparser[Haftarah.Customs] = ???
    }
  }

  private def parser(full: Boolean): Parser[Customs] = for {
    span <- spanParser
    parts <- new PartElement(span).seq()
    parts <- if (parts.isEmpty) ZIO.none else partsParser(parts).map(Some(_))
    customsElements <- new CustomElement(span).seq()
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

  final class CustomElement(ancestorSpan: BookSpanParsed) extends Element[(Set[Custom], Haftarah)]("custom") {
    override def contentParsable: Parsable[(Set[Custom], Haftarah)] = new Parsable[(Set[Custom], Haftarah)] {
      override def parser: Parser[(Set[Custom], Haftarah)] = for {
        n <- Attribute("n").required()
        bookSpanParsed <- spanParser.map(_.inheritFrom(ancestorSpan))
        parts <- new PartElement(bookSpanParsed).seq()
        result <- if (parts.isEmpty) ZIO.succeed[Haftarah](oneSpan(bookSpanParsed)) else partsParser(parts)
      } yield Custom.parse(n) -> result

      override def antiparser: Antiparser[(Set[Custom], Haftarah)] = ???
    }
  }

  private final class PartElement(ancestorSpan: BookSpanParsed) extends Element[WithNumber[BookSpan]]("part") {
    override def contentParsable: Parsable[WithNumber[BookSpan]] = new Parsable[WithNumber[BookSpan]] {
      override def parser: Parser[WithNumber[BookSpan]] =
        WithNumber.parse(spanParser.map(_.inheritFrom(ancestorSpan).resolve))

      override def antiparser: Antiparser[WithNumber[BookSpan]] = ???
    }
  }

  private def partsParser(parts: Seq[WithNumber[BookSpan]]): Parser[Haftarah] = for {
    _ <- WithNumber.checkConsecutive(parts, "part")
    _ <- Parser.check(parts.length > 1, "too short")
  } yield Haftarah(WithNumber.dropNumbers(parts))
}
