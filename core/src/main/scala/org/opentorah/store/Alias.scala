package org.opentorah.store

import org.opentorah.metadata.Names
import org.podval.xml.{XmlAst, XmlCodec, XmlError}

// TODO remove
final case class Alias(override val names: Names, to: String) extends Terminal derives CanEqual

object Alias:
  val codec: XmlCodec[Alias] = new XmlCodec[Alias]:
    override def elementName: String = "alias"
    override def isRecordLike: Boolean = true

    override def unsafeDecode[E: XmlAst](element: E): Alias =
      val to: String = element.get("to").map(_.trim).filter(_.nonEmpty).getOrElse:
        throw XmlError("Missing attribute 'to'")
      Alias(
        names = Names.fromDefaultName(element.get("n"), NameChildren.decode(element)),
        to = to
      )

    override def encodeNamed[E: XmlAst](elName: String, value: Alias): E =
      NameChildren.encode(elName, value.names, Seq("to" -> value.to))
