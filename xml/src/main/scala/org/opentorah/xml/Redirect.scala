package org.opentorah.xml

import java.net.URL

final class Redirect[A](val url: URL, element: Element[A]) {
  def parse: Parser[A] = element.parse(url)
  def followRedirects: Parser[A] = element.followRedirects.parse(url)
  def orRedirect: Parser[Redirect.Or[A]] = element.orRedirect.parse(url)
  def withRedirect(follow: Boolean): Parser[Redirect.Or[A]] = element.withRedirect(follow).parse(url)
}

object Redirect {
  type Or[A] = Either[Redirect[A], A]
}
