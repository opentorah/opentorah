package org.opentorah.tei

import org.opentorah.xml.{ElementTo, Parsable, Parser, Unparser}

final class LangUsage(
  val languages: Seq[Language]
)

object LangUsage extends ElementTo[LangUsage]("langUsage"):

  override def contentParsable: Parsable[LangUsage] = new Parsable[LangUsage]:

    override def parser: Parser[LangUsage] = for
      languages: Seq[Language] <- Language.seq()
    yield LangUsage(
      languages
    )

    override val unparser: Unparser[LangUsage] = Tei.concat(
      Language.seq(_.languages)
    )
