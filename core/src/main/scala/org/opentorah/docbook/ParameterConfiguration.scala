package org.opentorah.docbook

import org.opentorah.xml.{Element, Parsable, Parser, Unparser}

final class ParameterConfiguration(val name: String, val value: String)

// TODO clean up parsing/unparsing
object ParameterConfiguration extends Element[ParameterConfiguration]("parameter"):
  override def contentParsable: Parsable[ParameterConfiguration] = new Parsable[ParameterConfiguration]:

    override def parser: Parser[ParameterConfiguration] = for
      name: String <- Configuration.nameAttribute()
      value: String <- Configuration.valueAttribute()
    yield ParameterConfiguration(name, value)

    override def unparser: Unparser[ParameterConfiguration] = Unparser.concat[ParameterConfiguration](
      Configuration.nameAttribute(_.name),
      Configuration.valueAttribute(_.value)
    )
  
  def toMap(parameters: Seq[ParameterConfiguration]): Map[String, String] =
    parameters.map(parameter => parameter.name -> parameter.value).toMap

  def fromMap(parameters: Map[String, String]): Seq[ParameterConfiguration] =  
    parameters.toSeq.map((name, value) => ParameterConfiguration(name, value))
