package org.podval.calendar.tanach

import cats.implicits._
import org.digitaljudaica.metadata.WithNames
import org.digitaljudaica.xml.{From, Xml}
import org.podval.judaica.tanach.Torah
import org.podval.judaica.tanach.Torah.Maftir
import scala.xml.Elem

trait ParseMaftir { self: WithNames =>

  final lazy val maftir: Maftir =
    From.xml(maftirElement).parseDo(Xml.withName("maftir", Torah.spanParser.map(_.resolve.from(this))))

  protected def maftirElement: Elem
}
