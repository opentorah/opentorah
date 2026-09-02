package org.opentorah.metadata

import org.opentorah.util.Effects
import org.opentorah.xml.{Attribute, ContentType, ElementTo, Parsable, Parser, Text, Unparser}
import org.podval.xml.{XmlAst, XmlCodec, XmlError}
import zio.blocks.schema.{Modifier, Schema}

final case class Name(name: String, languageSpec: Language.Spec) derives CanEqual:
  def satisfies(spec: Language.Spec): Boolean =
    def satisfies[T](f: Language.Spec => Option[T])(using CanEqual[T, T]): Boolean = f(spec).isEmpty || (f(languageSpec) == f(spec))
    satisfies(_.language) && satisfies(_.isTransliterated) && satisfies(_.flavour)

object Name extends ElementTo[Name]("name"):
  private val nAttribute: Attribute[String] = Attribute("n")

  override def contentType: ContentType = ContentType.Characters

  override def contentParsable: Parsable[Name] = new Parsable[Name]:
    override def parser: Parser[Name] = for
      n: Option[String] <- nAttribute.optional()
      characters: Option[String] <- Text().optional()
      _ <- Effects.check(n.nonEmpty || characters.nonEmpty, "Both 'n' attribute and text are absent.")
      _ <- Effects.check(n.isEmpty  || characters.isEmpty, "Both 'n' attribute and text are present.")
      name: Option[String] = n.orElse(characters)
      languageSpec: Language.Spec <- Language.Spec()
    yield Name(
      name = name.get,
      languageSpec = languageSpec
    )

    override val unparser: Unparser[Name] = Unparser.concat(
      nAttribute.required(_.name),
      Language.Spec(_.languageSpec)
    )

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
      val fromN: Option[String] = data.n.map(_.trim).filter(_.nonEmpty)
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
