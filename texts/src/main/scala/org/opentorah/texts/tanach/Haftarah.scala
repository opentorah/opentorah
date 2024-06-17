package org.opentorah.texts.tanach

import org.opentorah.metadata.{HasName, Language, Names}
import org.opentorah.util.{Collections, Effects}
import org.opentorah.xml.{Attribute, ElementTo, From, Parsable, Parser, Unparser}
import zio.ZIO

// TODO de-case - and figure out why object Haftarah's creation becomes impossible if 'case' is removed here...
final case class Haftarah(override val spans: Seq[Haftarah.BookSpan]) extends Haftarah.Spans(spans) derives CanEqual:
  override def equals(other: Any): Boolean = this.spans == other.asInstanceOf[Haftarah].spans

object Haftarah extends WithBookSpans[Tanach.Prophets]:
  override type Many = Haftarah

  def toLanguageString(spans: Seq[BookSpan])(using spec: Language.Spec): String =
    Collections.group(spans, (span: BookSpan) => span.book)
      .map(bookSpans =>
        bookSpans.head.book.toLanguageString + " " + bookSpans.map(_.span.toLanguageString).mkString(", ")
      ).mkString("; ")

  override protected def getBook(name: String): Tanach.Prophets = Tanach.Prophets.forName(name)

  object Week extends ElementTo[(String, Customs)]("week"):
    private val elementParser = Haftarah.parser(full = true)

    override def contentParsable: Parsable[(String, Customs)] = new Parsable[(String, Customs)]:
      override def parser: Parser[(String, Customs)] = for
        name: String <- Names.defaultNameAttribute.required()
        result: Customs <- elementParser
      yield (name, result)

      override def unparser: Unparser[(String, Haftarah.Customs)] = ???

  lazy val haftarah: Map[Parsha, Customs] = Collections.mapValues(Parser.unsafeRun(HasName.load(
    from = From.resource(this),
    content = Week,
    keys = Parsha.valuesSeq,
    hasName = (metadata: (String, Customs), name: String) => metadata._1 == name
  )))(_._2)

  def element(full: Boolean): ElementTo[Customs] = new ElementTo[Customs]("haftarah"):
    override def contentParsable: Parsable[Customs] = new Parsable[Customs]:
      override def parser: Parser[Customs] = Haftarah.parser(full)
      override def unparser: Unparser[Haftarah.Customs] = ???

  private def parser(full: Boolean): Parser[Customs] = for
    bookSpanParsed: BookSpanParsed <- spanParser
    parts: Seq[WithNumber[BookSpan]] <- PartElement(bookSpanParsed).seq()
    partsOpt: Option[Haftarah] <- if parts.isEmpty then ZIO.none else partsParser(parts).map(Some(_))
    customsElements: Seq[(Set[Custom], Haftarah)] <- CustomElement(bookSpanParsed).seq()
  yield
    val customs: Custom.Of[Haftarah] = Custom.Of(customsElements, full = false)
    val common: Option[Haftarah] = if parts.isEmpty && customsElements.isEmpty then Some(oneSpan(bookSpanParsed)) else partsOpt

    val result: Map[Custom, Haftarah] = common.fold(customs.customs)(common =>
      require(customs.find(Custom.Common).isEmpty)
      customs.customs.updated(Custom.Common, common)
    )

    new Custom.Of(result, full = full)

  private def oneSpan(span: BookSpanParsed): Haftarah = Haftarah(Seq(span.resolve))

  final class CustomElement(ancestorSpan: BookSpanParsed) extends ElementTo[(Set[Custom], Haftarah)]("custom"):
    override def contentParsable: Parsable[(Set[Custom], Haftarah)] = new Parsable[(Set[Custom], Haftarah)]:
      override def parser: Parser[(Set[Custom], Haftarah)] = for
        n: String <- Attribute("n").required()
        bookSpanParsed: BookSpanParsed <- spanParser.map(_.inheritFrom(ancestorSpan))
        parts: Seq[WithNumber[BookSpan]] <- PartElement(bookSpanParsed).seq()
        result: Haftarah <- if parts.isEmpty then ZIO.succeed[Haftarah](oneSpan(bookSpanParsed)) else partsParser(parts)
      yield Custom.parse(n) -> result

      override def unparser: Unparser[(Set[Custom], Haftarah)] = ???

  private final class PartElement(ancestorSpan: BookSpanParsed) extends ElementTo[WithNumber[BookSpan]]("part"):
    override def contentParsable: Parsable[WithNumber[BookSpan]] = new Parsable[WithNumber[BookSpan]]:
      override def parser: Parser[WithNumber[BookSpan]] =
        WithNumber.parse(spanParser.map(_.inheritFrom(ancestorSpan).resolve))

      override def unparser: Unparser[WithNumber[BookSpan]] = ???

  private def partsParser(parts: Seq[WithNumber[BookSpan]]): Parser[Haftarah] = for
    _ <- WithNumber.checkConsecutive(parts, "part")
    _ <- Effects.check(parts.length > 1, "too short")
  yield Haftarah(WithNumber.dropNumbers(parts))
