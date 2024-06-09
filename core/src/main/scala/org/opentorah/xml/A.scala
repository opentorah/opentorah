package org.opentorah.xml

import org.opentorah.util.Files
import java.net.URI

// HTML anchor element
// TODO move to the html package
final class A(
  uri: Option[URI] = None,
  target: Option[String] = None,
  id: Option[String] = None,
  classes: Seq[String] = Seq.empty
):
  private def copy(
    uri: Option[URI] = uri,
    target: Option[String] = target,
    id: Option[String] = id,
    classes: Seq[String] = classes
  ): A = new A(
    uri,
    target,
    id,
    classes
  )

  def setId(value: String): A = copy(id = Some(value))

  def setTarget(value: Option[String]): A = value.fold(this)(setTarget)

  def setTarget(value: String): A = copy(target = Some(value))

  def addClass(value: String): A = copy(classes = classes :+ value)
  
  def setFragment(value: String): A = copy(uri = Some(
    uri.fold(URI(null, null, null, null, value))
    (uri => URI(uri.getScheme, uri.getAuthority, uri.getPath, uri.getQuery, value))
  ))

  def setQuery(value: String): A = copy(uri = Some(
    uri.fold(URI(null, null, null, "/", value))
    (uri => URI(uri.getScheme, uri.getAuthority, uri.getPath, value, uri.getFragment))
  ))

  def apply(text: String): Xml.Element = apply(Seq(Xml.mkText(text)))

  def apply(element: Xml.Element): Xml.Element = apply(Seq(element))

  def apply(xml: RawXml#Value): Xml.Element = apply(xml.content.nodes)

  def apply(children: Xml.Nodes): Xml.Element =
    <a
    href={uri.map(_.toString).orNull}
    target={target.orNull}
    class={if classes.isEmpty then null else classes.mkString(" ")}
    id={id.orNull}
    >{children}</a>

object A:
  def apply(path: Seq[String]): A = apply(URI(null, null, Files.mkUrl(path), null))

  def apply(uri: URI): A = new A(uri = Some(uri))
  
  def empty: A = new A()
