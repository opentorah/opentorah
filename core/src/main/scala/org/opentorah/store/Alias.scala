package org.opentorah.store

import org.opentorah.metadata.{Name, Names}
import org.podval.xml.{XmlAst, XmlCodec}
import zio.blocks.schema.{Modifier, Schema}

// TODO remove
final case class Alias(override val names: Names, to: String) extends Terminal derives CanEqual

object Alias:
  private final case class Data(
    @Modifier.config(XmlCodec.Attribute, "") n: Option[String] = None,
    @Modifier.config(XmlCodec.Element, "name") names: Seq[Name.Data] = Seq.empty,
    @Modifier.config(XmlCodec.Attribute, "") to: String
  ) derives CanEqual

  private object Data:
    given schema: Schema[Data] = Schema.derived
    val codec: XmlCodec[Data] = XmlCodec.derived

  val codec: XmlCodec[Alias] = new XmlCodec[Alias]:
    override def elementName: String = "alias"
    override def isRecordLike: Boolean = true

    override def unsafeDecode[E: XmlAst](element: E): Alias =
      val data: Data = Data.codec.unsafeDecode(element)
      Alias(Names.fromDefaultName(data.n, data.names.map(Name.fromData)), data.to)

    override def encodeNamed[E: XmlAst](elName: String, value: Alias): E =
      val default: Option[String] = value.names.getDefaultName
      Data.codec.encodeNamed(elName, Data(
        n = default,
        names = if default.isDefined then Seq.empty else value.names.names.map(Name.toData),
        to = value.to
      ))
