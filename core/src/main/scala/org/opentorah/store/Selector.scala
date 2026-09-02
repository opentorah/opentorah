package org.opentorah.store

import org.opentorah.metadata.{HasValues, Name, Named, Names}
import org.podval.xml.{XmlAst, XmlCodec, XmlParser}
import zio.blocks.schema.{Modifier, Schema}

// TODO introduce transparent (optional) selectors.
final case class Selector(
  override val names: Names,
  title: Option[String] // TODO replace with plural? Eliminate?
) extends Named derives CanEqual

object Selector extends HasValues.FindByName[Selector]:
  private final case class Data(
    @Modifier.config(XmlCodec.Attribute, "") n: Option[String] = None,
    @Modifier.config(XmlCodec.Element, "name") names: Seq[Name.Data] = Seq.empty,
    @Modifier.config(XmlCodec.Attribute, "") title: Option[String] = None
  ) derives CanEqual

  private object Data:
    given schema: Schema[Data] = Schema.derived
    val codec: XmlCodec[Data] = XmlCodec.derived

  val codec: XmlCodec[Selector] = new XmlCodec[Selector]:
    override def elementName: String = "selector"
    override def isRecordLike: Boolean = true

    override def unsafeDecode[E: XmlAst](element: E): Selector =
      val data: Data = Data.codec.unsafeDecode(element)
      Selector(Names.fromDefaultName(data.n, data.names.map(Name.fromData)), data.title)

    override def encodeNamed[E: XmlAst](elName: String, value: Selector): E =
      val default: Option[String] = value.names.getDefaultName
      Data.codec.encodeNamed(elName, Data(
        n = default,
        names = if default.isDefined then Seq.empty else value.names.names.map(Name.toData),
        title = value.title
      ))

  def valuesSeq: Seq[Selector] = values.toIndexedSeq

  lazy val values: Seq[Selector] = XmlParser.loadCatalog(this, codec)
