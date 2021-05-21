package org.opentorah.xml

import java.net.URL

// TODO just use XInclude?
final class Redirect[A](val url: URL, elements: Elements[A]) {
  def parse: Parser[A] = elements.parse(url)
  def followRedirects: Parser[A] = elements.followRedirects.parse(url)
  def orRedirect: Parser[Redirect.Or[A]] = elements.orRedirect.parse(url)
  def withRedirect(follow: Boolean): Parser[Redirect.Or[A]] = elements.withRedirect(follow).parse(url)
}

object Redirect {
  type Or[A] = Either[Redirect[A], A]
}
