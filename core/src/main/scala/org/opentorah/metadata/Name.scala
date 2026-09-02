package org.opentorah.metadata

import org.podval.xml.{XmlAst, XmlCodec, XmlError}
import zio.blocks.schema.{Modifier, Schema}

final case class Name(name: String, languageSpec: Language.Spec) derives CanEqual:
  def satisfies(spec: Language.Spec): Boolean =
    def satisfies[T](f: Language.Spec => Option[T])(using CanEqual[T, T]): Boolean = f(spec).isEmpty || (f(languageSpec) == f(spec))
    satisfies(_.language) && satisfies(_.isTransliterated) && satisfies(_.flavour)

object Name:
  private final case class Data(
    @Modifier.config(XmlCodec.Attribute, "") n: Option[String] = None,
    @Modifier.config(XmlCodec.Text, "") text: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "lang") lang: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "") transliterated: Option[Boolean] = None,
    @Modifier.config(XmlCodec.Attribute, "") flavour: Option[String] = None
  ) derives CanEqual

  private object Data:
    given schema: Schema[Data] = Schema.derived
    val codec: XmlCodec[Data] = XmlCodec.derived

  val codec: XmlCodec[Name] = new XmlCodec[Name]:
    override def elementName: String = "name"
    override def isRecordLike: Boolean = true

    override def unsafeDecode[E: XmlAst](element: E): Name =
      val data: Data = Data.codec.unsafeDecode(element)
      val fromN: Option[String] = data.n.map(_.trim)
      val fromText: Option[String] = data.text.map(_.trim).filter(_.nonEmpty)
      if fromN.isEmpty && fromText.isEmpty then throw XmlError("Both 'n' attribute and text are absent.")
      if fromN.isDefined && fromText.isDefined then throw XmlError("Both 'n' attribute and text are present.")
      Name(
        name = fromN.orElse(fromText).get,
        languageSpec = Language.Spec(
          language = data.lang.map(Language.getForDefaultName),
          isTransliterated = data.transliterated,
          flavour = data.flavour
        )
      )

    override def encodeNamed[E: XmlAst](elName: String, value: Name): E =
      Data.codec.encodeNamed(elName, Data(
        n = Some(value.name),
        text = None,
        lang = value.languageSpec.language.map(_.name),
        transliterated = value.languageSpec.isTransliterated,
        flavour = value.languageSpec.flavour
      ))
