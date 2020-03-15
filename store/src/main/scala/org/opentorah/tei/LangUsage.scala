package org.opentorah.tei

import org.opentorah.xml.Element
import scala.xml.Elem

final case class LangUsage(
  languages: Seq[Language]
)

object LangUsage extends Element[LangUsage](
  elementName = "langUsage",
  parser = for {
    languages <- Language.all
  } yield new LangUsage(
    languages
  )
) {
  override def toXml(value: LangUsage): Elem =
    <langUsage>{Language.toXml(value.languages)}</langUsage>
}
