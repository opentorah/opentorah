package org.opentorah.tei

import org.opentorah.xml.{Unparser, Element, Parsable, Parser}

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

    override val unparser: Unparser[LangUsage] = Tei.concat(
      Language.seq(_.languages)
    )
  }
}
