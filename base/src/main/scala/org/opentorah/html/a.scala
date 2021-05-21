package org.opentorah.html

import org.opentorah.util.Files
import org.opentorah.xml.{RawXml, Xml}
import java.net.URI

final case class a(
  uri: Option[URI] = None,
  target: Option[String] = None,
  id: Option[String] = None,
  classes: Seq[String] = Seq.empty,
  declareNamespace: Boolean = false
) {
  def setId(value: String): a = copy(id = Some(value))

  def setTarget(value: Option[String]): a = value.fold(this)(setTarget)

  def setTarget(value: String): a = copy(target = Some(value))

  def addClass(value: String): a = copy(classes = classes :+ value)

  def withNamespace: a = copy(declareNamespace = true)

  def setFragment(value: String): a = copy(uri = Some(
    uri.fold(new URI(null, null, null, null, value))
    (uri => new URI(uri.getScheme, uri.getAuthority, uri.getPath, uri.getQuery, value))
  ))

  def setQuery(value: String): a = copy(uri = Some(
    uri.fold(new URI(null, null, null, "/", value))
    (uri => new URI(uri.getScheme, uri.getAuthority, uri.getPath, value, uri.getFragment))
  ))

  def apply(text: String): Xml.Element = apply(Seq(Xml.mkText(text)))

  def apply(element: Xml.Element): Xml.Element = apply(Seq(element))

  def apply(xml: RawXml#Value): Xml.Element = apply(xml.xml)

  def apply(children: Xml.Nodes): Xml.Element = {
    val result: Xml.Element =
      <a
      href={uri.map(_.toString).orNull}
      target={target.orNull}
      class={if (classes.isEmpty) null else classes.mkString(" ")}
      id={id.orNull}
      >{children}</a>

    if (!declareNamespace) result else Html.namespace.default.declare(result)
  }
}

object a {
  def apply(path: Seq[String]): a = apply(new URI(null, null, Files.mkUrl(path), null))

  def apply(uri: URI): a = a(uri = Some(uri))
}
