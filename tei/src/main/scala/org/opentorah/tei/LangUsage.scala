package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parser}

final case class LangUsage(
  languages: Seq[Language]
)

object LangUsage extends Element[LangUsage]("langUsage") {

  override def parser: Parser[LangUsage] = for {
    languages <- Language.all
  } yield new LangUsage(
    languages
  )

  override val antiparser: Antiparser[LangUsage] = Tei.concat(
    Language.toXmlSeq(_.languages)
  )
}
