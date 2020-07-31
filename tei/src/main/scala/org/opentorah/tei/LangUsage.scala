package org.opentorah.tei

import org.opentorah.xml.{Attribute, Element, Parser}
import scala.xml.Elem

final case class LangUsage(
  languages: Seq[Language]
)

object LangUsage extends Element.WithToXml[LangUsage]("langUsage") {

  override protected def parser: Parser[LangUsage] = for {
    languages <- Language.all
  } yield new LangUsage(
    languages
  )

  override protected def attributes(value: LangUsage): Seq[Attribute.Value[_]] = Seq.empty

  override protected def content(value: LangUsage): Seq[Elem] =
    Language.toXml(value.languages)
}
