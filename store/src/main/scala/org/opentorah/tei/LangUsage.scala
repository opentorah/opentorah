package org.opentorah.tei

import org.opentorah.xml.{Element, ToXml}
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
) with ToXml[LangUsage] {
  override def toXml(value: LangUsage): Elem =
    <langUsage>{Language.toXml(value.languages)}</langUsage>
}
