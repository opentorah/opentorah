package org.opentorah.docbook

import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser}

final class OutputConfiguration(val format: String)

// TODO split out format and variant
// TODO clean up parsing/unparsing
object OutputConfiguration extends Element[OutputConfiguration]("output"):
  override def contentParsable: Parsable[OutputConfiguration] = new Parsable[OutputConfiguration]:
    private val formatAttribute: Attribute.Required[String] = Attribute("format").required

    override def parser: Parser[OutputConfiguration] = for
      format: String <- formatAttribute()
    yield OutputConfiguration(format)

    override def unparser: Unparser[OutputConfiguration] = Unparser.concat[OutputConfiguration](
      formatAttribute(_.format)
    )

  def toSet(output: Seq[OutputConfiguration]): Set[String] = output.map(_.format).toSet
  def fromSet(output: Set[String]): Seq[OutputConfiguration] = output.toSeq.map(OutputConfiguration(_))
