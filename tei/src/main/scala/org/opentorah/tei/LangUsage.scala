package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parser}

final case class LangUsage(
  languages: Seq[Language]
)

object LangUsage extends Element.WithToXml[LangUsage]("langUsage") {

  override protected def parser: Parser[LangUsage] = for {
    languages <- Language.all
  } yield new LangUsage(
    languages
  )

  override protected val antiparser: Antiparser[LangUsage] = Antiparser(
    Language.elementAntiparserSeq.compose[LangUsage](_.languages)
  )
}
