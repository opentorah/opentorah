package org.opentorah.html

import org.opentorah.util.Files
import org.opentorah.xml.{RawXml, ScalaXml}
import java.net.URI

final case class a(
  uri: Option[URI] = None,
  target: Option[String] = None,
  id: Option[String] = None,
  classes: Seq[String] = Seq.empty,
  declareNamespace: Boolean = false
):
  def setId(value: String): a = copy(id = Some(value))

  def setTarget(value: Option[String]): a = value.fold(this)(setTarget)

  def setTarget(value: String): a = copy(target = Some(value))

  def addClass(value: String): a = copy(classes = classes :+ value)

  def withNamespace: a = copy(declareNamespace = true)

  def setFragment(value: String): a = copy(uri = Some(
    uri.fold(URI(null, null, null, null, value))
    (uri => URI(uri.getScheme, uri.getAuthority, uri.getPath, uri.getQuery, value))
  ))

  def setQuery(value: String): a = copy(uri = Some(
    uri.fold(URI(null, null, null, "/", value))
    (uri => URI(uri.getScheme, uri.getAuthority, uri.getPath, value, uri.getFragment))
  ))

  def apply(text: String): ScalaXml.Element = apply(Seq(ScalaXml.mkText(text)))

  def apply(element: ScalaXml.Element): ScalaXml.Element = apply(Seq(element))

  def apply(xml: RawXml#Value): ScalaXml.Element = apply(xml.content.scalaXml)

  def apply(children: ScalaXml.Nodes): ScalaXml.Element =
    val result: ScalaXml.Element =
      <a
      href={uri.map(_.toString).orNull}
      target={target.orNull}
      class={if classes.isEmpty then null else classes.mkString(" ")}
      id={id.orNull}
      >{children}</a>

    if !declareNamespace then result else ScalaXml.declareNamespace(Html.namespace.default, result)

object a:
  def apply(path: Seq[String]): a = apply(URI(null, null, Files.mkUrl(path), null))

  def apply(uri: URI): a = a(uri = Some(uri))
