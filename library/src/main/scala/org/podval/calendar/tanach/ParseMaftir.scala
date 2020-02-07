package org.podval.calendar.tanach

import cats.implicits._
import org.digitaljudaica.metadata.{WithNames, Xml}
import org.podval.judaica.tanach.Torah
import org.podval.judaica.tanach.Torah.Maftir

import scala.xml.Elem

trait ParseMaftir { self: WithNames =>

  // TODO switch to Parser[A]
  final lazy val maftir: Maftir = Xml.runA(maftirElement, "maftir", parser)

  protected def maftirElement: Elem

  private val parser: Xml.Parser[Maftir] = for {
    bookSpanParsed <- Torah.parseSpanNg
    result = bookSpanParsed.resolve
  } yield result.from(this)
}
