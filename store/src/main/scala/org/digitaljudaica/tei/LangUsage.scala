package org.digitaljudaica.tei

import org.digitaljudaica.xml.Descriptor
import scala.xml.Elem

final case class LangUsage(
  languages: Seq[Language]
)

object LangUsage extends Descriptor[LangUsage](
  elementName = "langUsage",
  contentParser = for {
    languages <- Language.all
  } yield new LangUsage(
    languages
  )
) {
  override def toXml(value: LangUsage): Elem =
    <langUsage>{Language.toXml(value.languages)}</langUsage>
}
