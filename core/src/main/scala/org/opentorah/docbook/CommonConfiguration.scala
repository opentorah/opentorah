package org.opentorah.docbook

import org.opentorah.xml.{Element, Parsable, Parser, Unparser}

final class CommonConfiguration(
  name: String,
  val parameters: Map[String, String]
):
  val common: Common = Common.forName(name)


object CommonConfiguration extends Element[CommonConfiguration]("common"):

  override def contentParsable: Parsable[CommonConfiguration] = new Parsable[CommonConfiguration]:

    override def parser: Parser[CommonConfiguration] = for
      name: String <- Configuration.nameAttribute()
      parameters: Seq[ParameterConfiguration] <- ParameterConfiguration.seq()
    yield CommonConfiguration(
      name = name,
      parameters = ParameterConfiguration.toMap(parameters)
    )

    override def unparser: Unparser[CommonConfiguration] = Unparser.concat[CommonConfiguration](
      Configuration.nameAttribute(_.common.name),
      ParameterConfiguration.seq(configuration => ParameterConfiguration.fromMap(configuration.parameters))
    )
