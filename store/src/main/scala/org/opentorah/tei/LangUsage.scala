package org.opentorah.tei

import org.opentorah.xml.Descriptor
import scala.xml.Elem

final case class LangUsage(
  languages: Seq[Language]
)

object LangUsage extends Descriptor[LangUsage](
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
