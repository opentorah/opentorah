package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parsable, Parser}

final case class LangUsage(
  languages: Seq[Language]
)

object LangUsage extends Element[LangUsage]("langUsage") {

  override def contentParsable: Parsable[LangUsage] = new Parsable[LangUsage] {

    override def parser: Parser[LangUsage] = for {
      languages <- Language.seq()
    } yield new LangUsage(
      languages
    )

    override val antiparser: Antiparser[LangUsage] = Tei.concat(
      Language.seq(_.languages)
    )
  }
}
