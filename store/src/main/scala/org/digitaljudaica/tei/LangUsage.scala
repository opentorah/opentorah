package org.digitaljudaica.tei

import org.digitaljudaica.xml.Descriptor

final case class LangUsage(
  languages: Seq[Language]
)

object LangUsage extends Descriptor[LangUsage](
  elementName = "langUsage",
  contentParser = for {
    languages <- Language.all
  } yield new LangUsage(
    languages
  ),
  toXml = (value: LangUsage) => <langUsage>{Language.toXml(value.languages)}</langUsage>
)
