package org.opentorah.html

import org.opentorah.util.Files
import org.opentorah.xml.{RawXml, ScalaXml}
import java.net.URI

// HTML anchor element
final class A(
  uri: Option[URI] = None,
  target: Option[String] = None,
  id: Option[String] = None,
  classes: Seq[String] = Seq.empty,
  declareNamespace: Boolean = false // TODO eliminate; add to the <a> xmlns={namespace.uri}
):
  private def copy(
    uri: Option[URI] = uri,
    target: Option[String] = target,
    id: Option[String] = id,
    classes: Seq[String] = classes,
    declareNamespace: Boolean = declareNamespace
  ): A = new A(
    uri,
    target,
    id,
    classes,
    declareNamespace
  )

  def setId(value: String): A = copy(id = Some(value))

  def setTarget(value: Option[String]): A = value.fold(this)(setTarget)

  def setTarget(value: String): A = copy(target = Some(value))

  def addClass(value: String): A = copy(classes = classes :+ value)

  def withNamespace: A = copy(declareNamespace = true)

  def setFragment(value: String): A = copy(uri = Some(
    uri.fold(URI(null, null, null, null, value))
    (uri => URI(uri.getScheme, uri.getAuthority, uri.getPath, uri.getQuery, value))
  ))

  def setQuery(value: String): A = copy(uri = Some(
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

object A:
  def apply(path: Seq[String]): A = apply(URI(null, null, Files.mkUrl(path), null))

  def apply(uri: URI): A = new A(uri = Some(uri))
  
  def empty: A = new A()
