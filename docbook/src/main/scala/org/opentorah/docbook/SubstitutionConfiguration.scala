package org.opentorah.docbook

import org.opentorah.xml.{Element, Parsable, Parser, Unparser}

final class SubstitutionConfiguration(val name: String, val value: String)

// TODO clean up parsing/unparsing
object SubstitutionConfiguration extends Element[SubstitutionConfiguration]("substitution"):
  override def contentParsable: Parsable[SubstitutionConfiguration] = new Parsable[SubstitutionConfiguration]:

    override def parser: Parser[SubstitutionConfiguration] = for
      name: String <- Configuration.nameAttribute()
      value: String <- Configuration.valueAttribute()
    yield SubstitutionConfiguration(name, value)

    override def unparser: Unparser[SubstitutionConfiguration] = Unparser.concat[SubstitutionConfiguration](
      Configuration.nameAttribute(_.name),
      Configuration.valueAttribute(_.value)
    )

  def toMap(substitutions: Seq[SubstitutionConfiguration]): Map[String, String] =
    substitutions.map(substitution => substitution.name -> substitution.value).toMap

  def fromMap(substitutions: Map[String, String]): Seq[SubstitutionConfiguration] =
    substitutions.toSeq.map((name, value) => SubstitutionConfiguration(name, value))
