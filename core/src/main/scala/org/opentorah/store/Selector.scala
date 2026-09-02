package org.opentorah.store

import org.opentorah.metadata.{HasValues, Named, Names}
import org.podval.xml.{XmlAst, XmlCodec, XmlParser}

// TODO introduce transparent (optional) selectors.
final case class Selector(
  override val names: Names,
  title: Option[String] // TODO replace with plural? Eliminate?
) extends Named derives CanEqual

object Selector extends HasValues.FindByName[Selector]:
  // TODO why so much stuff? org.podval.xml not powerful enough?
  val codec: XmlCodec[Selector] = new XmlCodec[Selector]:
    override def elementName: String = "selector"
    override def isRecordLike: Boolean = true

    override def unsafeDecode[E: XmlAst](element: E): Selector =
      Selector(
        names = Names.fromDefaultName(element.get("n"), NameChildren.decode(element)),
        title = element.get("title")
      )

    override def encodeNamed[E: XmlAst](elName: String, value: Selector): E =
      NameChildren.encode(elName, value.names, value.title.toSeq.map("title" -> _))

  def valuesSeq: Seq[Selector] = values.toIndexedSeq

  lazy val values: Seq[Selector] = XmlParser.loadCatalog(this, codec)

// TODO why so much stuff? org.podval.xml not powerful enough?
private[store] object NameChildren:
  def decode[E: XmlAst](element: E): Seq[org.opentorah.metadata.Name] =
    org.opentorah.metadata.Name.codec.decodeChildren(element).fold(error => throw error, identity)

  def encode[E: XmlAst](
    elName: String,
    names: Names,
    extraAttributes: Seq[(String, String)]
  ): E =
    val ast: XmlAst[E] = summon[XmlAst[E]]
    val default: Option[String] = names.getDefaultName
    val attributes: Seq[(String, String)] = default.toSeq.map("n" -> _) ++ extraAttributes
    val children: ast.Nodes =
      if default.isDefined then Seq.empty
      else names.names.map(name => org.opentorah.metadata.Name.codec.encode(name))
    ast.element(elName, attributes, children)
