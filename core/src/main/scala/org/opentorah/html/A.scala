package org.opentorah.html

import org.opentorah.util.Files
import org.opentorah.xml.{Atom, Element, Nodes, RawXml}
import java.net.URI

// HTML anchor element
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

  def apply(text: String): Element = apply(Seq(Atom(text)))

  def apply(element: Element): Element = apply(Seq(element))

  def apply(xml: RawXml#Value): Element = apply(xml.content)

  def apply(children: Nodes): Element =
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
